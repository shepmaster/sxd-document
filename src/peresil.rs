pub trait StrParseExt {
    fn end_of_start_rest<F1, F2>(&self, is_first: F1, is_rest: F2) -> Option<usize>
        where F1: Fn(char) -> bool,
              F2: Fn(char) -> bool;
    fn end_of_literal(&self, expected: &str) -> Option<usize>;
}

impl<'a> StrParseExt for &'a str {
    fn end_of_start_rest<F1, F2>(&self, is_first: F1, is_rest: F2) -> Option<usize>
        where F1: Fn(char) -> bool,
              F2: Fn(char) -> bool
    {
        let mut positions = self.char_indices();

        match positions.next() {
            Some((_, c)) if is_first(c) => (),
            Some((_, _)) => return None,
            None => return None,
        };

        let mut positions = positions.skip_while(|&(_, c)| is_rest(c));
        match positions.next() {
            Some((offset, _)) => Some(offset),
            None => Some(self.len()),
        }
    }

    fn end_of_literal(&self, expected: &str) -> Option<usize> {
        if self.starts_with(expected) {
            Some(expected.len())
        } else {
            None
        }
    }
}

pub struct BestFailure<'a, E> {
    pub failure: Option<Progress<'a, E>>
}

impl<'a, E> BestFailure<'a, E> {
    pub fn new() -> BestFailure<'a, E> {
        BestFailure {
            failure: None,
        }
    }

    pub fn with(failure: Progress<'a, E>) -> BestFailure<'a, E> {
        BestFailure {
            failure: Some(failure),
        }
    }

    pub fn push(&mut self, failure: Progress<'a, E>) {
        if let Some(ref old) = self.failure {
            if failure.point.offset <= old.point.offset {
                return;
            }
        }

        self.failure = Some(failure)
    }

    pub fn pop(self) -> Progress<'a, E> {
        self.failure.expect("No errors found")
    }
}

#[derive(Debug,Clone,PartialEq,Copy)]
pub struct Point<'a> {
    pub offset: usize,
    pub s: &'a str,
}

impl<'a> Point<'a> {
    pub fn to(&self, other: Point<'a>) -> &'a str {
        let len = other.offset - self.offset;
        &self.s[..len]
    }

    pub fn slice_at(&self, position: usize) -> Progress<'a, &'a str> {
        Progress {
            data: &self.s[..position],
            point: Point { offset: self.offset + position,
                           s: &self.s[position..] }
        }
    }

    pub fn consume_to<E>(&self, l: Option<usize>) -> Result<'a, &'a str, E> {
        match l {
            None => Result::Failure(Progress{point: self.clone(), data: None}),
            Some(position) => Result::Success(self.slice_at(position)),
        }
    }

    pub fn consume_literal<E>(&self, literal: &str) -> Result<'a, &'a str, E> {
        self.consume_to(self.s.end_of_literal(literal))
    }

    pub fn consume_identifier<T, E>(&self, identifiers: &[Identifier<T>]) -> Result<'a, T, E>
        where T: Clone
    {
        for &(id_str, ref id_data) in identifiers.iter() {
            let result = self.consume_literal(id_str);
            if let Result::Success(..) = result {
                return result.map(|_| id_data.clone());
            }
        }

        Result::Failure(Progress {point: *self, data: None})
    }
}

pub type Identifier<'a, T> = (&'a str, T);

pub struct Progress<'a, T> {
    pub point: Point<'a>,
    pub data: T,
}

impl<'a, T> Progress<'a, T> {
    pub fn to_tuple(self) -> (T, Point<'a>) {
        (self.data, self.point)
    }

    pub fn map<F, B>(self, f: F) -> Progress<'a, B>
        where F: FnOnce(T) -> B
    {
        Progress { data: f(self.data), point: self.point }
    }
}

pub enum Result<'a, T, E> {
    Success(Progress<'a, T>),
    Partial{success: Progress<'a, T>, failure: Progress<'a, Option<E>>},
    Failure(Progress<'a, Option<E>>),
}

impl<'a, T, E> Result<'a, T, E> {
    pub fn success(data: T, point: Point<'a>) -> Result<'a, T, E> {
        Result::Success(Progress { data: data, point: point })
    }

    pub fn failure(data: Option<E>, point: Point<'a>) -> Result<'a, T, E> {
        Result::Failure(Progress { data: data, point: point })
    }

    // Pattern: zero-or-one
    pub fn optional(self, point: Point<'a>) -> (Option<T>, Point<'a>) {
        match self {
            Result::Success(progress) |
            Result::Partial{ success: progress, .. } =>
                (Some(progress.data), progress.point),
            Result::Failure(_) =>
                (None, point),
        }
    }

    // Pattern: alternate
    pub fn or_else<F>(self, f: F) -> Result<'a, T, E>
        where F: FnMut() -> Result<'a, T, E>
    {
        let mut f = f;
        match self {
            Result::Success(..) => self,
            Result::Partial{ failure: fail, .. } |
            Result::Failure(fail) => {
                let next = f();
                match next {
                    Result::Success(..) => next,
                    Result::Partial{ failure: next_fail, .. } |
                    Result::Failure(next_fail) => {
                        let mut bf = BestFailure::with(fail);
                        bf.push(next_fail);
                        Result::Failure(bf.pop())
                    }
                }
            },
        }
    }

    pub fn map<F, B>(self, f: F) -> Result<'a, B, E>
        where F: FnOnce(T) -> B
    {
        match self {
            Result::Success(prog) =>
                Result::Success(prog.map(f)),
            Result::Partial{success: s, failure: ff} => {
                Result::Partial{ success: s.map(f), failure: ff}
            },
            Result::Failure(pf) =>
                Result::Failure(pf),
        }
    }

    pub fn set_error(self, error: E) -> Result<'a, T, E> {
        match self {
            Result::Success(..) =>
                self,
            Result::Partial{ success: s, failure: f } =>
                Result::Partial{ success: s, failure: f.map(move |_| Some(error)) },
            Result::Failure(f) =>
                Result::Failure(f.map(move |_| Some(error))),
        }
    }
}

#[macro_export]
macro_rules! try_parse(
    ($e:expr) => ({
        match $e {
            ::document::peresil::Result::Success(progress) =>
                progress.to_tuple(),
            ::document::peresil::Result::Partial{ failure: pf, .. } |
            ::document::peresil::Result::Failure(pf) =>
                return ::document::peresil::Result::Failure(pf),
        }
    });
    ($e:expr, $err:expr) => ({
        match $e {
            ::document::peresil::Result::Success(progress) =>
                progress.to_tuple(),
            ::document::peresil::Result::Partial{ failure: pf, .. } |
            ::document::peresil::Result::Failure(pf) => {
                let fail = ::document::peresil::Progress{ data: Some($err), ..pf };
                return ::document::peresil::Result::Failure(fail);
            },
        }
    });
);

#[macro_export]
macro_rules! try_partial_parse(
    ($e:expr) => ({
        match $e {
            ::document::peresil::Result::Success(progress) =>
                (progress.data, ::document::peresil::BestFailure::new(), progress.point),
            ::document::peresil::Result::Partial{ success: s, failure: pf } =>
                (s.data, ::document::peresil::BestFailure::with(pf), s.point),
            ::document::peresil::Result::Failure(pf) =>
                return ::document::peresil::Result::Failure(pf),
        }
    })
);

#[macro_export]
macro_rules! try_resume_after_partial_failure(
    ($partial:expr, $e:expr) => ({
        match $e {
            ::document::peresil::Result::Success(progress) =>
                progress.to_tuple(),
            ::document::peresil::Result::Partial{ failure: pf, .. } |
            ::document::peresil::Result::Failure(pf) => {
                let mut partial = $partial;
                partial.push(pf);
                return ::document::peresil::Result::Failure(partial.pop())
            },
        }
    });
);

// Pattern: zero-or-many
pub fn zero_or_more<'a, T, E, P>(point: Point<'a>, parser: P) -> Result<'a, Vec<T>, E>
    where P: FnMut(Point<'a>) -> Result<'a, T, E>
{
    let mut err;
    let mut data = Vec::new();
    let mut point = point;
    let mut parser = parser;

    loop {
        let (d, next_start) = match parser(point) {
            Result::Success(progress) =>
                progress.to_tuple(),
            Result::Partial{ failure: pf, .. } |
            Result::Failure(pf) => {
                err = Some(pf);
                break
            }
        };

        data.push(d);
        point = next_start;
    }

    Result::Partial {
        success: Progress { data: data, point: point },
        failure: err.unwrap()
    }
}
