import typing
from dataclasses import dataclass

import numpy as np

from part1 import Direction, Coord

Grid = np.ndarray[tuple[int, int], np.dtype[np.bool_]]

Vec = np.ndarray[tuple[int], np.dtype[np.int_]]
Face = tuple[int, int, int]

Rotation = np.ndarray[tuple[int, int], np.dtype[np.int_]]

RI = np.identity(3, dtype=np.int_)
Rx = np.array(((1, 0, 0), (0, 0, -1), (0, 1, 0)), dtype=np.int_)
Ry = np.array(((0, 0, 1), (0, 1, 0), (-1, 0, 0)), dtype=np.int_)
Rz = np.array(((0, -1, 0), (1, 0, 0), (0, 0, 1)), dtype=np.int_)

DIRECTION_ROTS: dict[Direction, Rotation] = {
    Direction.RIGHT: Ry,
    Direction.DOWN: Rx,
    Direction.LEFT: np.linalg.inv(Ry).astype(np.int_),
    Direction.UP: np.linalg.inv(Rx).astype(np.int_),
}


class FoldedNet:
    def __init__(
        self,
        face_size: int,
        sector_faces: dict[Coord, Face],
        face_rotations: dict[Face, Rotation],
    ):
        self.face_size = face_size
        self.sector_faces = sector_faces
        # Pre-compute and cache inverses
        self.face_rotations: dict[Face, tuple[Rotation, Rotation]] = {
            face: (rot, np.linalg.inv(rot).astype(np.int_))
            for face, rot in face_rotations.items()
        }

    def get_sector_rotation(self, sector: Coord) -> Rotation:
        return self.face_rotations[self.sector_faces[sector]][0]

    def get_face_sector(self, face_norm: Face) -> Coord:
        for s, f in self.sector_faces.items():
            if f == face_norm:
                return s
        raise ValueError(f'Invalid face {face_norm}')

    def map_vec_face_to_face(
        self, src_point: Vec, src_face: Face, dst_face: Face
    ) -> Vec:
        # Find dst_point such that
        # dst_rotation * dst_point = src_rotation * src_point
        # dst_point = dst_rotation^-1 * src_rotation * src_point
        src_rot, _inv = self.face_rotations[src_face]
        _non_inv, dst_rot_inv = self.face_rotations[dst_face]
        base_point: Vec = np.matmul(src_rot, src_point)
        return np.matmul(dst_rot_inv, base_point)

    def coord_to_vec(self, src_coord: Coord) -> Vec:
        """Convert 2D coord to vector on the Z+ face"""
        zero_indexed = np.array(
            (src_coord.col, src_coord.row, self.face_size-1)
        )
        # Transform by 2x+1 to stay in integer domain, whether face_size is
        # even or odd, then shift back to the center
        centered = 2 * zero_indexed - (self.face_size - 1)
        # Flip y coordinate direction
        centered[1] *= -1
        return centered

    def vec_to_coord(self, src_vec: Vec) -> Coord:
        # Flip y coordinate direction
        src_vec[1] *= -1
        col, row, z = (src_vec + (self.face_size - 1)) // 2
        assert z == self.face_size - 1, f'Vector not on a face'
        return Coord(row=row, col=col)

    def get_dest_face(self, src_face: Face, direction: Direction) -> Face:
        direction_vec_2d = Coord.from_direction(direction)
        # Row direction is flipped to get Y direction
        direction_vec = (direction_vec_2d.col, -direction_vec_2d.row, 0)
        src_rotation, _inv = self.face_rotations[src_face]
        src_direction_vec: Vec = np.matmul(src_rotation, direction_vec)
        dst_face = tuple(src_direction_vec)
        assert dst_face in self.face_rotations, f'Computed invalid face: {dst_face}'
        return dst_face

    def get_dest_direction(self, src_face: Face, dst_face: Face) -> Direction:
        _non_inv, dst_rot_inv = self.face_rotations[dst_face]
        # Going from src -> dst, the direction relative to dst is always just
        # opposite the src face norm
        dst_direction_vec = - np.matmul(dst_rot_inv, src_face)
        dst_dir_col, dst_dir_row, _ = dst_direction_vec
        direction = Direction.from_delta(-dst_dir_row, dst_dir_col)
        if direction is None:
            raise ValueError(f'Invalid direction vector: {dst_direction_vec}')
        return direction

    def compute_new_face_coord(
        self,
        src_pos: Coord,
        src_sector: Coord,
        direction: Direction,
    ) -> tuple[Coord, Coord, Direction]:
        src_vec = self.coord_to_vec(src_pos)
        src_face = self.sector_faces[src_sector]
        dst_face = self.get_dest_face(src_face, direction)
        dst_vec = self.map_vec_face_to_face(src_vec, src_face, dst_face)

        return (
            self.get_face_sector(dst_face),
            self.vec_to_coord(dst_vec),
            self.get_dest_direction(src_face, dst_face),
        )


def get_adj_dir(src: Coord, dst: Coord) -> typing.Optional[Direction]:
    return Direction.from_delta(dst.row - src.row, dst.col - src.col)


def fold_net(grid: Grid, face_size: int) -> FoldedNet:
    face_rotations: dict[Face, Rotation] = {}
    sector_faces: dict[Coord, Face] = {}

    sector_locs = [Coord(*loc) for loc in np.argwhere(grid)]

    def visit(c: Coord, face: Face, rotation: Rotation):
        sector_faces[c] = face
        face_rotations[face] = rotation

        for new_sector in sector_locs:
            if new_sector in sector_faces:
                continue

            if (d := get_adj_dir(c, new_sector)) is not None:
                new_rot = np.matmul(rotation, DIRECTION_ROTS[d])
                visit(
                    new_sector,
                    tuple(np.matmul(new_rot, (0, 0, 1))),
                    new_rot,
                )

    visit(sector_locs[0], (0, 0, 1), RI)

    assert len(sector_faces) == len(sector_locs), 'Missed some sectors'

    return FoldedNet(face_size, sector_faces, face_rotations)
