//! file-extension: stl

Vec3d = struct {
    x: f32le,
    y: f32le,
    z: f32le,
};

Triangle = struct {
    /// Normal vector
    normal: Vec3d,
    /// Coordinates of the vertices
    vertices: [Vec3d; 3u8],
};

/// https://en.wikipedia.org/wiki/STL_(file_format)
Stl = struct {
    /// Generally ignored
    header: [u8; 80u8],
    /// Number of triangles that follow
    num_triangles: u32le,
    /// The triangle data
    triangles: [Triangle; num_triangles],
};
