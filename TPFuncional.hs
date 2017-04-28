
-- Punto 1: Modelado de Cliente

data Cliente = UnCliente { nombre :: [char],
                           resistencia :: Int,
						   amigos :: [Cliente] } deriving (Show, Eq)
						   
-- Punto 2: Modelo Rodri, Marcos, Cristian y Ana

rodri = UnCliente Rodri 55 []
marcos = UnCliente Marcos 40 [rodri]
cristian = UnCliente Cristian 2 []
ana = UnCliente Ana 120 [marcos, rodri]

-- Punto 3: Funcion comoEsta

comoEsta cliente
	|(estaFresco cliente) = "Fresco"
	|((not (estaFresco cliente))) && (masDeUnAmigo cliente) = "Piola"
	|otherwise = "Duro" 
	
estaFresco cliente = (resistencia cliente) > 50

masDeUnAmigo cliente = (length(amigos cliente)) > 1 

-- Punto 4: 
reconocerAmigo cliente amigo
	|(nombre cliente == nombre amigo) = error "No se puede ser amigo de uno mismo"
	| elem (nombre amigo) (amigos cliente) = error "Ya es amigo"
	| otherwise = UnCliente { (nombre cliente) (resistencia cliente) (amigos cliente):(nombre amigo)}
	
-- Punto 5:
grogXD cliente = UnCliente {(nombre cliente) 0 (amigos cliente)}

klusener gusto cliente = UnCliente {(nombre cliente) (resistencia cliente - length gusto) (amigos cliente)}

tintico cliente = UnCliente {(nombre cliente) (resistencia cliente + length (amigos cliente) (amigos cliente)}