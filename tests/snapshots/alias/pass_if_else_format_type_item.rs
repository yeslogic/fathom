// This file is automatically @generated by ddl 0.1.0
// It is not intended for manual editing.

pub const FOO: bool = true;

#[derive(Copy, Clone)]
pub enum Enum0 {
    True(f64),
    False(f32),
}

#[derive(Copy, Clone)]
pub struct Test {
    inner: Enum0,
}

impl Test {
    pub fn inner(&self) -> Enum0 {
        self.inner
    }
}

impl ddl_rt::Format for Test {
    type Host = Test;
}

impl<'data> ddl_rt::ReadFormat<'data> for Test {
    fn read(reader: &mut ddl_rt::FormatReader<'data>) -> Result<Test, ddl_rt::ReadError> {
        let inner = if FOO { 
            Enum0::True(reader.read::<ddl_rt::F64Be>()?)
        } else { 
            Enum0::False(reader.read::<ddl_rt::F32Be>()?)
        };

        Ok(Test {
            inner,
        })
    }
}