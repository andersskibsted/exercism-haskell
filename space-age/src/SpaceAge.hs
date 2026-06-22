module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
year :: Float
year = 365.25 * 24 * 3600

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = (seconds / year) / 0.2408467
ageOn Venus seconds = (seconds / year) / 0.61519726
ageOn Earth seconds = seconds / year
ageOn Mars seconds = (seconds / year) / 1.8808158
ageOn Jupiter seconds = (seconds / year) / 11.862615
ageOn Saturn seconds = (seconds / year) / 29.447498
ageOn Uranus seconds = (seconds / year) / 84.016846
ageOn Neptune seconds = (seconds / year) / 164.79132
