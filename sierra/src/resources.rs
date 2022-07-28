use std::collections::HashMap;

use crate::graph::Identifier;

pub(crate) type ResourceMap = HashMap<Identifier, i64>;

pub(crate) fn resource_usage(id: Identifier, count: i64) -> ResourceMap {
    ResourceMap::from([(id, count)])
}

pub(crate) fn resource_add(r1: &ResourceMap, r2: &ResourceMap) -> ResourceMap {
    chain!(
        r1.iter()
            .map(|(r, change)| (r.clone(), change + r2.get(r).unwrap_or(&0),)),
        r2.iter()
            .map(|(r, change)| (r.clone(), r1.get(r).unwrap_or(&0) + change))
    )
    .collect()
}

#[cfg(test)]
mod function {
    use super::*;
    use crate::graph::Identifier;
    #[test]
    fn test_add() {
        assert_eq!(
            resource_add(
                &resource_usage(Identifier("g".to_string()), 1),
                &resource_usage(Identifier("g".to_string()), 1)
            ),
            resource_usage(Identifier("g".to_string()), 2)
        );
        assert_eq!(
            resource_add(
                &resource_usage(Identifier("g1".to_string()), 2),
                &resource_usage(Identifier("g2".to_string()), 1)
            ),
            ResourceMap::from([
                (Identifier("g1".to_string()), 2),
                (Identifier("g2".to_string()), 1)
            ])
        );
    }
}
