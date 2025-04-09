use reflect_to::ToTypescript;

mod example_data;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut ts_generator = ToTypescript::default();

    ts_generator.add_type::<example_data::User>()?;

    println!("{}", ts_generator.generate()?);

    Ok(())
}
