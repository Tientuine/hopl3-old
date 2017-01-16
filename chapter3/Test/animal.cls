% correct result: 18

class Organism extends object
    field nutrients
    method initialize(nut) set nutrients = nut
    method getNutrients() nutrients

class Animal extends Organism
    method initialize() super initialize(5)
    method eat(food)
        let nut = send food getNutrients()
        in set nutrients = -(nutrients,-(0,nut))

class Plant extends Organism
    method initialize() super initialize(3)

class Herbivore extends Animal
    method initialize() super initialize()
    method eat(food)
        if instanceof food Plant
        then super eat(food)
        else super eat(0)

class Carnivore extends Animal
    method initialize() super initialize()
    method eat(food)
        if instanceof food Animal
        then super eat(food)
        else super eat(0)

class Omnivore extends Animal
    method initialize () super initialize()
    method eat (food)
        if instanceof food Organism
        then super eat (food)
        else super eat (0)

let grass = new Plant()
    grasshopper = new Herbivore()
    frog = new Omnivore()
    snake = new Carnivore()
in begin
    send grasshopper eat(grass); % 5 +  3 = 8
    send frog eat(grasshopper);  % 5 +  8 = 13
    send snake eat(frog);        % 5 + 13 = 18
    send snake getNutrients()    % 18
end

