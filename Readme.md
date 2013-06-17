The project is mainly made up of two parts.

The first part is about a circular structure that uses the SIMD approach to perform the inverse operations and resolutions of linear systems or simply to make a series of pivot on the main diagonal.

The second one is about a flower structure where a single dispatcher is responsible for sending the job to multiple workers (add, subtract or product) and waits for the result.

# Circle

In the first case the structure as has been said is circular, each ring of the circle (dispatcher) is responsible for preparing the matrix (exchange rows and possibly divide it in the right way) and then communicate with a series of Worker performing pivot operation.

In fact every Dispatcher performs, through the Workers, the pivot operation on a single line and then pass the array to the next Dispatcher that will deal with the following lines.

More than a matrix can be treated simultaneously.

Instructions:

Start the circle system using the module launcher:

    Launcher:start()

> This command will takes care of launching the server that will serves the works to the ring


Launches the Supervisor who creates the structure:

    Launcher:startSupervisor({circle, D, W})

> **D** and **W** shall be specified as two integers representing respectively the D number of Dispatchers (so we know how many matrices can be treated at the same time) and W the number of Workers for each Dispatcher (give an idea of ​​the level of parallelism on each pivot ).

Import a 100x100 matrix of random previously saved file:

    A = smm:import("mr100100.txt")

and start the operation with

    Launcher:sendwork(TypeOfWork, A).

> Starts the job, TypeOfWork is an atom and should have one of the following values ​​(*reverse* | *symlin* | *pivot*). The system will print the number assigned to our work (**Nwork**). 

We could retrieve the result with:

    Launcher:getWork(Nwork)

**Nwork** must be exactly the number communicated to us by the previous command.


# Flower

Here we have only one Dispatcher that will divide the matrix between the workers depending on the requested operation. At the end of operation the result will be assembled from the workers results

In this case it is necessary to start the first supervisor:

    Flowersv:start({flower, W}).

> **W** should be and integer indicating how many workers to start

From here on you can send job to the dispatcher with:

    Flower_dispatcher:operation4Row(prod, A, B);

> **A** and **B** represent the matrices on which to perform the operation
> The first term is an atom that can have the values ​​(*prod* | *sum* | *sub*)
