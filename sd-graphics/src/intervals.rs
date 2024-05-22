#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Interval<I> {
    pub start: I,
    pub end: I,
}

impl<I> Interval<I> {
    pub fn new(start: I, end: I) -> Self {
        Interval { start, end }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Intervals<I, T>(Vec<(Interval<I>, T)>);

impl<I, T> Intervals<I, T> {
    #[must_use]
    pub fn new() -> Self {
        Intervals(Vec::new())
    }
}

impl<I, T> Default for Intervals<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Ord + Copy, T: Clone> Intervals<I, T> {
    pub fn query<'a>(&'a self, interval: &'a Interval<I>) -> impl Iterator<Item = &'a T> + 'a {
        let start = self.0.partition_point(|(i, _)| i.end < interval.start); // first such that i.end >= interval.start
        let end = self.0.partition_point(|(i, _)| i.start <= interval.end); // first such that i.start > interval.end

        self.0[start..end].iter().map(|(_, v)| v)
    }

    pub fn insert(&mut self, interval: Interval<I>, value: T) {
        let start = self.0.partition_point(|(i, _)| i.start < interval.start);
        let end = self.0.partition_point(|(i, _)| i.end <= interval.end);

        if start == end + 1 {
            let (i, v) = self.0.remove(end);
            self.0.splice(
                end..end,
                [
                    (Interval::new(i.start, interval.start), v.clone()),
                    (interval, value),
                    (Interval::new(interval.end, i.end), v),
                ],
            );
        } else {
            self.0.splice(start..end, [(interval, value)]);
            if start != 0 {
                self.0[start - 1].0.end = self.0[start - 1].0.end.min(interval.start);
            }
            if start + 1 < self.0.len() {
                self.0[start + 1].0.start = self.0[start + 1].0.start.max(interval.end);
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::Intervals;
    use crate::intervals::Interval;

    #[test]
    fn disjoint_intervals() {
        let mut ints = Intervals::new();
        let mut ints2 = Intervals::new();

        ints.insert(Interval::new(0, 1), 0);
        ints.insert(Interval::new(2, 3), 1);

        ints2.insert(Interval::new(2, 3), 1);
        ints2.insert(Interval::new(0, 1), 0);

        assert_eq!(ints, ints2);
    }

    #[test]
    fn insert_middle() {
        let mut ints = Intervals::new();
        ints.insert(Interval::new(0, 3), 0);
        ints.insert(Interval::new(1, 2), 1);

        assert_eq!(
            ints,
            Intervals(vec![
                (Interval::new(0, 1), 0),
                (Interval::new(1, 2), 1),
                (Interval::new(2, 3), 0)
            ])
        );
    }

    #[test]
    fn insert_start() {
        let mut ints = Intervals::new();
        ints.insert(Interval::new(0, 2), 0);
        ints.insert(Interval::new(1, 3), 1);

        assert_eq!(
            ints,
            Intervals(vec![(Interval::new(0, 1), 0), (Interval::new(1, 3), 1)])
        );
    }

    #[test]
    fn insert_end() {
        let mut ints = Intervals::new();
        ints.insert(Interval::new(1, 3), 1);
        ints.insert(Interval::new(0, 2), 0);

        assert_eq!(
            ints,
            Intervals(vec![(Interval::new(0, 2), 0), (Interval::new(2, 3), 1)])
        );
    }

    #[test]
    fn insert_between() {
        let mut ints = Intervals::new();
        ints.insert(Interval::new(0, 1), 0);
        ints.insert(Interval::new(4, 5), 2);
        ints.insert(Interval::new(2, 3), 1);

        assert_eq!(
            ints,
            Intervals(vec![
                (Interval::new(0, 1), 0),
                (Interval::new(2, 3), 1),
                (Interval::new(4, 5), 2)
            ])
        );
    }

    #[test]
    fn query() {
        let ints = Intervals(vec![
            (Interval::new(1, 2), 0),
            (Interval::new(5, 8), 1),
            (Interval::new(9, 10), 2),
        ]);
        assert_eq!(ints.query(&Interval::new(11, 12)).count(), 0);
        assert_eq!(ints.query(&Interval::new(3, 4)).count(), 0);
        assert_eq!(
            ints.query(&Interval::new(0, 4))
                .cloned()
                .collect::<Vec<_>>(),
            vec![0]
        );
        assert_eq!(
            ints.query(&Interval::new(6, 7))
                .cloned()
                .collect::<Vec<_>>(),
            vec![1]
        );
        assert_eq!(
            ints.query(&Interval::new(3, 7))
                .cloned()
                .collect::<Vec<_>>(),
            vec![1]
        );
        assert_eq!(
            ints.query(&Interval::new(1, 5))
                .cloned()
                .collect::<Vec<_>>(),
            vec![0, 1]
        );
        assert_eq!(
            ints.query(&Interval::new(0, 12))
                .cloned()
                .collect::<Vec<_>>(),
            vec![0, 1, 2]
        );
    }
}
