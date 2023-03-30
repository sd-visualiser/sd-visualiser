pub(crate) struct ConcatIter<I: Iterator> {
    iter: I,
    sub_iter: Option<<I as Iterator>::Item>,
}

impl<I> ConcatIter<I>
where
    I: Iterator,
    <I as Iterator>::Item: Iterator,
{
    fn try_next(&mut self) -> Option<<<I as Iterator>::Item as Iterator>::Item> {
	self.sub_iter.as_mut()?.next()
    }
}

impl<I> Iterator for ConcatIter<I>
where
    I: Iterator,
    <I as Iterator>::Item: Iterator,
{
    type Item = <<I as Iterator>::Item as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
	self.try_next().or_else(|| {
	    self.sub_iter = Some(self.iter.next()?);
	    self.next()
	})
    }
}

pub(crate) fn concat_iter<I: Iterator>(iter: I) -> ConcatIter<I> {
    ConcatIter {
        iter,
        sub_iter: None,
    }
}
