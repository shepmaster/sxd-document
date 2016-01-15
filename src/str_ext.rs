#[cfg(feature = "unstable")]
use std::str::pattern::{Pattern,Searcher};

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum SplitType<'a> {
    Match(&'a str),
    Delimiter(&'a str),
}

#[cfg(feature = "unstable")]
pub struct SplitKeepingDelimiter<'p, P>
    where P: Pattern<'p>
{
    searcher: P::Searcher,
    start: usize,
    saved: Option<usize>,
}

#[cfg(feature = "unstable")]
impl<'p, P> Iterator for SplitKeepingDelimiter<'p, P>
    where P: Pattern<'p>,
{
    type Item = SplitType<'p>;

    fn next(&mut self) -> Option<SplitType<'p>> {
        if self.start == self.searcher.haystack().len() {
            return None;
        }

        if let Some(end_of_match) = self.saved.take() {
            let s = &self.searcher.haystack()[self.start..end_of_match];
            self.start = end_of_match;
            return Some(SplitType::Delimiter(s));
        }

        match self.searcher.next_match() {
            Some((start, end)) => {
                if self.start == start {
                    let s = &self.searcher.haystack()[start..end];
                    self.start = end;
                    Some(SplitType::Delimiter(s))
                } else {
                    let s = &self.searcher.haystack()[self.start..start];
                    self.start = start;
                    self.saved = Some(end);
                    Some(SplitType::Match(s))
                }
            },
            None => {
                let s = &self.searcher.haystack()[self.start..];
                self.start = self.searcher.haystack().len();
                Some(SplitType::Match(s))
            },
        }
    }
}

#[cfg(feature = "unstable")]
pub trait SplitKeepingDelimiterExt: ::std::ops::Index<::std::ops::RangeFull, Output = str> {
    fn split_keeping_delimiter<P>(&self, pattern: P) -> SplitKeepingDelimiter<P>
        where P: for <'a> Pattern<'a>
    {
        SplitKeepingDelimiter { searcher: pattern.into_searcher(&self[..]), start: 0, saved: None }
    }
}

#[cfg(not(feature = "unstable"))]
pub struct SplitKeepingDelimiter<'a, F> {
    haystack: &'a str,
    chars: F,
    start: usize,
    saved: Option<usize>,
}

#[cfg(not(feature = "unstable"))]
impl<'a, F> Iterator for SplitKeepingDelimiter<'a, F>
    where F: Fn(char) -> bool
{
    type Item = SplitType<'a>;

    fn next(&mut self) -> Option<SplitType<'a>> {
        if self.start == self.haystack.len() {
            return None;
        }

        if let Some(end_of_match) = self.saved.take() {
            let s = &self.haystack[self.start..end_of_match];
            self.start = end_of_match;
            return Some(SplitType::Delimiter(s));
        }

        let tail = &self.haystack[self.start..];

        match tail.find(&self.chars) {
            Some(start) => {
                let start = self.start + start;
                let end = start + 1; // Super dangerous! Assume we are only one byte long
                if self.start == start {
                    let s = &self.haystack[start..end];
                    self.start = end;
                    Some(SplitType::Delimiter(s))
                } else {
                    let s = &self.haystack[self.start..start];
                    self.start = start;
                    self.saved = Some(end);
                    Some(SplitType::Match(s))
                }
            },
            None => {
                let s = &self.haystack[self.start..];
                self.start = self.haystack.len();
                Some(SplitType::Match(s))
            },
        }
    }
}

#[cfg(not(feature = "unstable"))]
pub trait SplitKeepingDelimiterExt: ::std::ops::Index<::std::ops::RangeFull, Output = str> {
    fn split_keeping_delimiter<F>(&self, chars: F) -> SplitKeepingDelimiter<F>
        where F: Fn(char) -> bool
    {
        SplitKeepingDelimiter { haystack: &self[..], chars: chars, start: 0, saved: None }
    }
}

impl SplitKeepingDelimiterExt for str {}

#[cfg(test)]
mod test {
    use super::{SplitKeepingDelimiterExt};

    #[test]
    fn split_with_delimiter() {
        use super::SplitType::*;
        let delims = |b| b == ',' || b == ';';
        let items: Vec<_> = "alpha,beta;gamma".split_keeping_delimiter(delims).collect();
        assert_eq!(&items, &[Match("alpha"), Delimiter(","), Match("beta"), Delimiter(";"), Match("gamma")]);
    }

    #[test]
    fn split_with_delimiter_allows_consecutive_delimiters() {
        use super::SplitType::*;
        let delims = |b| b == ',' || b == ';';
        let items: Vec<_> = ",;".split_keeping_delimiter(delims).collect();
        assert_eq!(&items, &[Delimiter(","), Delimiter(";")]);
    }
}
