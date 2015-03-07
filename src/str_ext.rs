use std::str::{Pattern,Searcher};

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum SplitType<'a> {
    Match(&'a str),
    Delimiter(&'a str),
}

struct SplitKeepingDelimiter<'p, P>
    where P: Pattern<'p>
{
    searcher: P::Searcher,
    start: usize,
    saved: Option<usize>,
}

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

pub trait SplitKeepingDelimiterExt: ::std::ops::Index<::std::ops::RangeFull, Output = str> {
    fn split_keeping_delimiter<P>(&self, pattern: P) -> SplitKeepingDelimiter<P>
        where P: for <'a> Pattern<'a>
    {
        SplitKeepingDelimiter { searcher: pattern.into_searcher(&self[..]), start: 0, saved: None }
    }
}

impl SplitKeepingDelimiterExt for str {}

#[cfg(test)]
mod test {
    use super::{SplitKeepingDelimiterExt};

    #[test]
    fn split_with_delimiter() {
        use super::SplitType::*;
        let items: Vec<_> = "alpha,beta;gamma".split_keeping_delimiter(&[',', ';'][..]).collect();
        assert_eq!(&items, &[Match("alpha"), Delimiter(","), Match("beta"), Delimiter(";"), Match("gamma")]);
    }

    #[test]
    fn split_with_delimiter_allows_consecutive_delimiters() {
        use super::SplitType::*;
        let items: Vec<_> = ",;".split_keeping_delimiter(&[',', ';'][..]).collect();
        assert_eq!(&items, &[Delimiter(","), Delimiter(";")]);
    }
}
