# Changelog

## [0.2.3] - 2025-08-29

### Added

- New token types: `Dot` (`.`), `DoubleDot` (`..`), `At` (`@`)
- Enhanced lexer logic for better dot handling
- Support for method calls (`obj.method`), ranges (`1..10`), and decorators (`@decorator`)
- New example demonstrating dot token functionality

### Changed

- Improved number parsing to work better with range operators
- Dot handling now has proper precedence over number parsing

### Performance

- Minimal performance impact (~3% regression) for significant new functionality
