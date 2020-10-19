list_type = type([])

def send_recv( sock, req ):
    req_s = umsgpack.packb( req )
    sock.send( req_s )
    rep_s = sock.recv()
    rep = umsgpack.unpackb( rep_s )
    return rep

def ok_else_exn( rep ):
    assert( type(rep) == list_type )
    assert( len(rep) == 2 )
    if rep[0] == u"Ok":
        return rep[1]
    else:
        raise Exception(rep[1])


def zscore(sock, num_features, num_bins):
    req = [u"ZScore", {u"num_features" : num_features, u"num_bins" : num_bins }]
    rep = send_recv( sock, req )
    return ok_else_exn( rep )

def train(sock, learning_rate, lamb):
    req = [u"Train", {
        u"learning_rate": float(learning_rate),
        u"lambda": float (lamb)
    }]
    rep = send_recv(sock, req )
    return ok_else_exn( rep )

def eval_loss( sock ):
    req = u"EvalLoss"
    rep = send_recv( sock, req )
    return ok_else_exn( rep )

def observation(yx):
    y = yx[0]
    x = yx[1]
    x = [float(xi) for xi in x]
    return [bool(y), x]

def observations( obss ):
    return [observation(obs) for obs in obss]

def add( sock, kind, obss ):
    req = [kind, observations(obss)]
    rep = send_recv( sock, req )
    return ok_else_exn( rep )

def addZ( sock, obss ):
    return add( sock, u"AddZ", obss )

def addT( sock, obss ):
    return add( sock, u"AddT", obss )

def addE( sock, obss ):
    return add( sock, u"AddE", obss )

def loss( sock ):
    req = u"Loss"
    rep = send_recv( sock, req )
    return ok_else_exn( rep )

def model( sock ):
    req = u"Model"
    rep = send_recv( sock, req )
    return ok_else_exn( rep )


from csv import reader

def read_csv( path ):
    ch = open( path )
    rows = reader( ch )
    obss = []
    for row in rows:
        if row[0] == 'Y':
            y = True
        elif row[0] == 'N':
            y = False
        else:
            assert( False )
        # convert from string to float
        x = [xi for xi in map( float, row[1:])]
        assert( len(x) == 6 )
        obss.append([y, x])
    return obss


# https://stackoverflow.com/questions/8991506/iterate-an-iterator-by-chunks-of-n-in-python?lq=1
def chunk(iterable, chunk_size):
    """Generate sequences of `chunk_size` elements from `iterable`."""
    iterable = iter(iterable)
    while True:
        chunk = []
        try:
            for _ in range(chunk_size):
                chunk.append( next(iterable) )
            yield chunk
        except StopIteration:
            if chunk:
                yield chunk
            break

def add_n_at_a_time( add_f, socket, obss, n ):
    for n_obs in chunk( obss, n ):
        add_f( socket, n_obs )

def addZ1( socket, obss, n ):
    add_n_at_a_time( addZ, socket, obss, n )

def addE1( socket, obss, n ):
    add_n_at_a_time( addE, socket, obss, n )

def addT1( socket, obss, n ):
    add_n_at_a_time( addT, socket, obss, n )


import umsgpack
import zmq
context = zmq.Context()
socket = context.socket(zmq.DEALER)
socket.connect("tcp://localhost:8765")

obss = read_csv( "../higgs-1M.csv")
chunk_size = 2950

rep = zscore( socket, 6, 10 )
rep = addZ1( socket, obss, chunk_size )
rep = train( socket, 1e-6, 1e-2 )
for epoch in range(100):
    print("epoch", epoch)
    rep = addT1( socket, obss, chunk_size )
    rep = eval_loss( socket )
    rep = addE1( socket, obss, chunk_size )
    rep = loss( socket ); print("loss=", rep)
    rep = model( socket ); print("model=", rep)
