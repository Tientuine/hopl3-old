% correct result: 3

class Node extends object
    field data
    field next
    field hasnext
    method initialize(x,xs,z)
        begin
            set data = x;
            set next = xs;
            set hasnext = z
        end
    method add(x)
        if zero?(hasnext)
            then let n = new Node(x,0,0)
                 in begin
                        set next = n;
                        set hasnext = 1
                    end
            else send next add(x)
    method get(i)
        if zero?(i)
            then data
            else send next get(-(i,1))

class LinkedList extends object
    field head
    field nonempty
    method initialize()
        begin
            set head = 0;
            set nonempty = 0
        end
    method add(x)
        if zero?(nonempty)
            then begin set head = new Node(x,0,0); set nonempty = 1 end
            else send head add(x)
    method get(i)
        send head get(i)

let xs = new LinkedList()
in begin
      send xs add(5);
      send xs add(4);
      send xs add(3);
      send xs add(2);
      send xs add(1);
      send xs get(2)
   end

