#[contract]
mod SomeTest {
    use option::OptionTrait;
    use serde::Serde;

    #[derive(Copy, Drop)]
    struct Position {
        x: felt,
        y: felt,
    }

    impl StorageAccessPosition of starknet::StorageAccess::<Position> {
        fn read(
            address_domain: felt, base: starknet::StorageBaseAddress
        ) -> starknet::SyscallResult::<Position> {
            Result::Ok(
                Position {
                    x: starknet::StorageAccess::<felt>::read(address_domain, base)?,
                    y: starknet::StorageAccess::<felt>::read(address_domain, base)?,
                }
            )
        }
        fn write(
            address_domain: felt, base: starknet::StorageBaseAddress, value: Position
        ) -> starknet::SyscallResult::<()> {
            starknet::StorageAccess::<felt>::write(address_domain, base, value.x)?;
            starknet::StorageAccess::<felt>::write(address_domain, base, value.y)
        }
    }

    impl PositionSerde of Serde::<Position> {
        fn serialize(ref serialized: Array::<felt>, input: Position) {
            Serde::serialize(ref serialized, input.x);
            Serde::serialize(ref serialized, input.y);
        }
        fn deserialize(ref serialized: Array::<felt>) -> Option::<Position> {
            Option::Some(
                Position {
                    x: Serde::deserialize(ref serialized)?, y: Serde::deserialize(ref serialized)?, 
                }
            )
        }
    }

    struct Storage {
        name: Position, 
    }


    #[view]
    fn get_name() -> Position {
        name::read()
    }

    fn set_name(test: Position) {
        name::write(test);
    }
}


#[test]
fn test_lamouche() {
    // SomeTest::set_name(SomeTest::Position { x: 1, y: 2,  });
    let a = SomeTest::get_name();
    // assert(a.x == 2, 'x');
}
