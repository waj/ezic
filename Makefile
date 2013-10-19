
zones:
	wget "http://www.iana.org/time-zones/repository/tzdata-latest.tar.gz"
	mkdir -p priv/tzdata
	tar -xvzf tzdata-latest.tar.gz -C priv/tzdata
	rm tzdata-latest.tar.gz
	ulimit -n 1024
	erl -pa ebin -s ezic_generator generate -s erlang halt
	erlc -o ebin zones/*.erl
