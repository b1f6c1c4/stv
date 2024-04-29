# Single transferable vote (STV)

This programs computes the multi-winner electoral system STV (Wright system specifically).

References:
- [Wikipedia on STV](https://en.wikipedia.org/wiki/Single_transferable_vote)
- [Wikipedia on Wright system](https://en.wikipedia.org/wiki/Wright_system)

## Design principals

- Minimizes wasted votes: votes cast for a candidate with no chance of winning.
- If a candidate is excluded from the count, all ballots are treated as if that candidate had never stood.
- If a candidate has achieved the quota, they retain a fixed proportion of the value of every vote received, and transfers the remainder to the next non-excluded continuing candidate, the value of the retained total equalling the quota.

## TODO & Limitations

- Computation speed & memory footprint
- Test cases
- Database connection

