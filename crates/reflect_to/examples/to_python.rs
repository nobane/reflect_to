// crates/reflect_to/examples/to_python.rs
use reflect_to::ToPython;

mod example_data;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut generator = ToPython::default();

    generator.add_type::<example_data::User>()?;

    println!("{}", generator.generate()?);

    Ok(())
}
