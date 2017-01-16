% result: 15

class Vehicle extends object
    field name
    field speed
    method initialize(nm)
        begin
            set name  = nm;
            set speed = 0
        end
    method toString() name

class Car extends Vehicle
    field name
    field cylinders
    method initialize(make,model,ncyl)
        begin
            super initialize(make);
            set name = model;
            set cylinders = ncyl
        end
    method toString()
        let make  = super toString()
            model = name
        in -(make,-(0,model))

let myBadassRide = new Car(5,10,4)
in send myBadassRide toString()

