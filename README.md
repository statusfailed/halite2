# Halite Bot

A bot and client library for [halite.io](https://www.halite.io).

Currently done:

- [ ] Port of Nuka
- [x] Types
- [x] Networking
  - [x] Parsers
  - [x] Tests
  - [x] Commands
- [ ] Geometry
  - [x] `calculateDistanceBetween :: Entity -> Entity -> Double`
  - [x] `lineIntersectsCircle :: Line -> Circle -> Maybe (Point, Point)`
  - [x] `segmentIntersectsCircle`
  - [ ] `obstaclesBetween :: GameMap -> Entity -> Entity -> [Entity]`
- [ ] Game functions
  - [ ] ship.closestPointTo(entity)
  - [ ] ship.navigate(position, speed)
  - [ ] tests
- [ ] Framework
  - [ ] StateT Writer (old commands overwritten by new)
  - [ ] ship.dock(planetId) ???
  - [ ] entity typeclass?
- [ ] Test framework
  - [x] Switch to Tasty
  - [ ] Add SmallCheck tests for geometry
