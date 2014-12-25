#![macro_escape]

#[deriving(Copy)]
pub struct BestFailure<'a> {
    pub failure: Option<ParseFailure<'a>>
}

impl<'a> BestFailure<'a> {
    pub fn new() -> BestFailure<'a> {
        BestFailure {
            failure: None,
        }
    }

    pub fn with(failure: ParseFailure<'a>) -> BestFailure<'a> {
        BestFailure {
            failure: Some(failure),
        }
    }

    pub fn push(&mut self, failure: ParseFailure<'a>) {
        if let Some(old) = self.failure {
            if failure.point.offset <= old.point.offset {
                return;
            }
        }

        self.failure = Some(failure)
    }

    pub fn pop(&self) -> ParseFailure<'a> {
        self.failure.expect("No errors found")
    }
}

#[deriving(Show,Clone,PartialEq,Copy)]
pub struct Point<'a> {
    pub offset: uint,
    pub s: &'a str,
}

impl<'a> Point<'a> {
    pub fn slice_at(&self, position: uint) -> (&'a str, Point<'a>) {
        (self.s.slice_to(position), Point{offset: self.offset + position,
                                          s: self.s.slice_from(position)})
    }

    pub fn consume_to(&self, l: Option<uint>) -> ParseResult<'a, &'a str> {
        match l {
            None => ParseResult::Failure(ParseFailure{point: self.clone()}),
            Some(position) => {
                let (val, point) = self.slice_at(position);
                ParseResult::Success(val, point)
            },
        }
    }
}

#[deriving(Copy)]
pub struct ParseFailure<'a> {
    pub point: Point<'a>,
}

pub enum ParseResult<'a, T> {
    Success(T, Point<'a>),
    Partial(T, ParseFailure<'a>, Point<'a>),
    Failure(ParseFailure<'a>),
}

macro_rules! try_parse(
    ($e:expr) => ({
        match $e {
            ::document::peresil::ParseResult::Success(val, point) =>
                (val, point),
            ::document::peresil::ParseResult::Partial(_, pf, _) |
            ::document::peresil::ParseResult::Failure(pf) =>
                return ::document::peresil::ParseResult::Failure(pf),
        }
    })
);

macro_rules! try_partial_parse(
    ($e:expr) => ({
        match $e {
            ::document::peresil::ParseResult::Success(v, point) =>
                (v, ::document::peresil::BestFailure::new(), point),
            ::document::peresil::ParseResult::Partial(val, pf, point) =>
                (val, ::document::peresil::BestFailure::with(pf), point),
            ::document::peresil::ParseResult::Failure(pf) =>
                return ::document::peresil::ParseResult::Failure(pf),
        }
    })
);

macro_rules! try_resume_after_partial_failure(
    ($partial:expr, $e:expr) => ({
        match $e {
            ::document::peresil::ParseResult::Success(val, point) =>
                (val, point),
            ::document::peresil::ParseResult::Partial(_, pf, _) |
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
            ::document::peresil::ParseResult::Success(val, point) =>
                (Some(val), point),
            ::document::peresil::ParseResult::Partial(val, _, point) =>
                (Some(val), point),
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
        [$parser:expr -> $transformer:expr],
        $([$parser_rest:expr -> $transformer_rest:expr],)*
    }) => (
        match $parser($start) {
            ::document::peresil::ParseResult::Success(val, point) =>
                ::document::peresil::ParseResult::Success($transformer(val), point),
            ::document::peresil::ParseResult::Partial(_, pf, _) |
            ::document::peresil::ParseResult::Failure(pf) => {
                $errors.push(pf);
                parse_alternate_rec!($start, $errors, {
                    $([$parser_rest -> $transformer_rest],)*
                })
            },
        }
    );
);

macro_rules! parse_alternate(
    ($start:expr, {
        $([$parser_rest:expr -> $transformer_rest:expr],)*
    }) => ({
        let mut errors = ::document::peresil::BestFailure::new();
        parse_alternate_rec!($start, errors, {
            $([$parser_rest -> $transformer_rest],)*
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
                ::document::peresil::ParseResult::Success(val, point) =>
                    (val, point),
                ::document::peresil::ParseResult::Partial(_, pf, _) |
                ::document::peresil::ParseResult::Failure(pf) => {
                    err = Some(pf);
                    break
                }
            };

            start = next_start;
        }

        ::document::peresil::ParseResult::Partial((), err.unwrap(), start)
    }};
);
