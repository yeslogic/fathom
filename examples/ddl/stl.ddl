//! endian: le
//! file-extension: stl

Vec3d = {
    x: f32,
    y: f32,
    z: f32,
};

Triangle = {
    /// Normal vector
    normal: Vec3d,
    /// Coordinates of the vertices
    vertices: [Vec3d; 3],
};

/// https://en.wikipedia.org/wiki/STL_(file_format)
Stl = {
    /// Generally ignored
    header: [u8; 80],
    /// Number of triangles that follow
    num_triangles: u32,
    /// The triangle data
    triangles: [Triangle; num_triangles],
};
