
-- Punto 1: Modelado de Cliente

data Cliente = UnCliente { resistencia :: Int,
						   amigos :: [Cliente] } deriving (Show, Eq)
						   
-- Punto 2: Modelo Rodri, Marcos, Cristian y Ana

rodri = UnCliente 55 []
marcos = UnCliente 40 [rodri]
cristian = UnCliente 2 []
ana = UnCliente 120 [marcos, rodri]

-- Punto 3: Funcion comoEsta

comoEsta cliente
	|(estaFresco cliente) = "Fresco"
	|((not (estaFresco cliente))) && (masDeUnAmigo cliente) = "Piola"
	|otherwise = "Duro" 
	
estaFresco cliente = (resistencia cliente) > 50

masDeUnAmigo cliente = (length(amigos cliente)) > 1 

-- Punto 4: 