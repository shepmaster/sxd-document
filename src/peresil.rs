#![macro_escape]

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

#[deriving(Show,Clone,PartialEq,Copy)]
pub struct Point<'a> {
    pub offset: uint,
    pub s: &'a str,
}

impl<'a> Point<'a> {
    pub fn to(&self, other: Point<'a>) -> &'a str {
        let len = other.offset - self.offset;
        self.s.slice_to(len)
    }

    pub fn slice_at(&self, position: uint) -> Progress<'a, &'a str> {
        Progress {
            data: self.s.slice_to(position),
            point: Point { offset: self.offset + position,
                           s: self.s.slice_from(position) }
        }
    }

    pub fn consume_to<E>(&self, l: Option<uint>) -> ParseResult<'a, &'a str, E> {
        match l {
            None => ParseResult::Failure(Progress{point: self.clone(), data: None}),
            Some(position) => ParseResult::Success(self.slice_at(position)),
        }
    }
}

pub struct Progress<'a, T> {
    pub point: Point<'a>,
    pub data: T,
}

impl<'a, T> Progress<'a, T> {
    pub fn to_tuple(self) -> (T, Point<'a>) {
        (self.data, self.point)
    }

    pub fn map<F, B>(self, f: F) -> Progress<'a, B>
        where F: Fn(T) -> B
    {
        Progress { data: f(self.data), point: self.point }
    }
}

pub enum ParseResult<'a, T, E> {
    Success(Progress<'a, T>),
    Partial{success: Progress<'a, T>, failure: Progress<'a, Option<E>>},
    Failure(Progress<'a, Option<E>>),
}

impl<'a, T, E> ParseResult<'a, T, E> {
    pub fn success(data: T, point: Point<'a>) -> ParseResult<'a, T, E> {
        ParseResult::Success(Progress { data: data, point: point })
    }

    pub fn map<F, B>(self, f: F) -> ParseResult<'a, B, E>
        where F: Fn(T) -> B
    {
        match self {
            ParseResult::Success(prog) =>
                ParseResult::Success(prog.map(f)),
            ParseResult::Partial{success: s, failure: ff} => {
                ParseResult::Partial{ success: s.map(f), failure: ff}
            },
            ParseResult::Failure(pf) =>
                ParseResult::Failure(pf),
        }
    }
}

macro_rules! try_parse(
    ($e:expr) => ({
        match $e {
            ::document::peresil::ParseResult::Success(progress) =>
                progress.to_tuple(),
            ::document::peresil::ParseResult::Partial{ failure: pf, .. } |
            ::document::peresil::ParseResult::Failure(pf) =>
                return ::document::peresil::ParseResult::Failure(pf),
        }
    })
);

macro_rules! try_partial_parse(
    ($e:expr) => ({
        match $e {
            ::document::peresil::ParseResult::Success(progress) =>
                (progress.data, ::document::peresil::BestFailure::new(), progress.point),
            ::document::peresil::ParseResult::Partial{ success: s, failure: pf } =>
                (s.data, ::document::peresil::BestFailure::with(pf), s.point),
            ::document::peresil::ParseResult::Failure(pf) =>
                return ::document::peresil::ParseResult::Failure(pf),
        }
    })
);

macro_rules! try_resume_after_partial_failure(
    ($partial:expr, $e:expr) => ({
        match $e {
            ::document::peresil::ParseResult::Success(progress) =>
                progress.to_tuple(),
            ::document::peresil::ParseResult::Partial{ failure: pf, .. } |
            ::document::peresil::ParseResult::Failure(pf) => {
                let mut partial = $partial;
                partial.push(pf);
                return ::document::peresil::ParseResult::Failure(partial.pop())
            },
        }
    });
);

// Pattern: zero-or-one
macro_rules! parse_optional(
    ($parser:expr, $start:expr) => ({
        match $parser {
            ::document::peresil::ParseResult::Success(progress) |
            ::document::peresil::ParseResult::Partial{ success: progress, .. } =>
                (Some(progress.data), progress.point),
            ::document::peresil::ParseResult::Failure(_) =>
                (None, $start),
        }
    })
);

// Pattern: alternate
macro_rules! parse_alternate_rec(
    ($start:expr, $errors:expr, {}) => ({
        ::document::peresil::ParseResult::Failure($errors.pop())
    });
    ($start:expr, $errors:expr, {
        $parser:expr,
        $($parser_rest:expr,)*
    }) => ({
        let result = $parser($start);
        match result {
            ::document::peresil::ParseResult::Success(..) =>
                result,
            ::document::peresil::ParseResult::Partial{ failure: pf, .. } |
            ::document::peresil::ParseResult::Failure(pf) => {
                $errors.push(pf);
                parse_alternate_rec!($start, $errors, {
                    $($parser_rest,)*
                })
            },
        }
    });
);

macro_rules! parse_alternate(
    ($start:expr, {
        $($parser_rest:expr,)*
    }) => ({
        let mut errors = ::document::peresil::BestFailure::new();
        parse_alternate_rec!($start, errors, {
            $($parser_rest,)*
        })
    });
);

// Pattern: zero-or-more
macro_rules! parse_zero_or_more(
    ($start:expr, $parser:expr) => {{
        let mut err;

        let mut start = $start;
        loop {
            let (_, next_start) = match $parser(start) {
                ::document::peresil::ParseResult::Success(progress) =>
                    progress.to_tuple(),
                ::document::peresil::ParseResult::Partial{ failure: pf, ..} |
                ::document::peresil::ParseResult::Failure(pf) => {
                    err = Some(pf);
                    break
                }
            };

            start = next_start;
        }

        ::document::peresil::ParseResult::Partial {
            success: ::document::peresil::Progress { data: (), point: start },
            failure: err.unwrap()
        }
    }};
);
