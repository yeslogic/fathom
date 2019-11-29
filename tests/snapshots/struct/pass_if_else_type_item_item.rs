// This file is automatically @generated by ddl 0.1.0
// It is not intended for manual editing.

pub const IS_BE: bool = true;

#[derive(Copy, Clone)]
pub enum Enum0 {
    True(f32),
    False(f32),
}

#[derive(Copy, Clone)]
pub struct Bar {
    inner: Enum0,
}

impl Bar {
    pub fn inner(&self) -> Enum0 {
        self.inner
    }
}

impl ddl_rt::Format for Bar {
    type Host = Bar;
}

impl<'data> ddl_rt::ReadFormat<'data> for Bar {
    fn read(reader: &mut ddl_rt::FormatReader<'data>) -> Result<Bar, ddl_rt::ReadError> {
        let inner = if IS_BE { 
            Enum0::True(reader.read::<ddl_rt::F32Be>()?)
        } else { 
            Enum0::False(reader.read::<ddl_rt::F32Le>()?)
        };

        Ok(Bar {
            inner,
        })
    }
}

#[derive(Copy, Clone)]
pub struct Test {
    bar: Bar,
}

impl Test {
    pub fn bar(&self) -> Bar {
        self.bar
    }
}

impl ddl_rt::Format for Test {
    type Host = Test;
}

impl<'data> ddl_rt::ReadFormat<'data> for Test {
    fn read(reader: &mut ddl_rt::FormatReader<'data>) -> Result<Test, ddl_rt::ReadError> {
        let bar = reader.read::<Bar>()?;

        Ok(Test {
            bar,
        })
    }
}