# Halite Bot

A bot and client library for [halite.io](https://www.halite.io).

Currently done:

- [ ] Port of Nuka
- [x] Types
- [x] Networking
  - [x] Parsers
  - [x] Tests
  - [x] Commands
- [x] Geometry
  - [x] `calculateDistanceBetween :: Entity -> Entity -> Double`
  - [x] `lineIntersectsCircle :: Line -> Circle -> Maybe (Point, Point)`
  - [x] `segmentIntersectsCircle`
  - [x] `obstaclesBetween :: GameMap -> Entity -> Entity -> [Entity]`
- [ ] Game functions
  - [x] ship.closestPointTo(entity)
  - [x] ship.navigate(position, speed)
  - [ ] `navigateTo` tests
  - [x] `ship.can_dock(planetId)`
  - [x] `ship.dock(planetId)`
  - [ ] tests
- [ ] Framework
  - [ ] StateT Writer (old commands overwritten by new)
  - [x] entity typeclass
- [ ] Test framework
  - [x] Switch to Tasty
  - [ ] Add SmallCheck tests for geometry
