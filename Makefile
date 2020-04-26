prod: src/*.elm
	npx elm make --optimize --output=assets/index.html src/Player.elm
	npx elm make --optimize --output=assets/admin.html src/Admin.elm
	cp src/bingo.css assets/bingo.css
