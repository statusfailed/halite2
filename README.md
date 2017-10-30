# Halite Bot

A bot and client library for [halite.io](https://www.halite.io).

Currently done:

- [x] Types
- [x] Networking
  - [x] Parsers
  - [x] Tests
  - [x] Commands
- [ ] Utility/Geometry functions
  - [ ] entity typeclass?
  - [x] lineIntersectsCircle
  - [x] entity.calculateDistanceBetween(entity)
  - [ ] ship.dock(planetId) ???
  - [ ] ship.navigate(position, speed)
  - [ ] ship.closestPointTo(entity)
  - [ ] tests
- [ ] Test framework
  - [ ] Switch to Tasty
  - [ ] Add QuickCheck tests for geometry
- [ ] Framework
  - [ ] StateT Writer (old commands overwritten by new)
- [ ] Port of Nuka
