from typing import Optional, List
from pydantic import BaseModel
from app.schemas.example import Example
from app.schemas.author import Author


class FragmentBase(BaseModel):
    title: Optional[str] = None


class FragmentCreate(FragmentBase):
    author_id: int


class FragmentUpdate(FragmentBase):
    pass


class Fragment(FragmentBase):
    id: int
    slug: str
    title: str
    author: Author
    examples: List[Example]

    class Config:
        orm_mode = True
