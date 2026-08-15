dotnet publish -c Release -o ./publish
rsync -avz --delete ./publish/ lambda:/var/www/hypercat.einarwh.no/
ssh lambda "systemctl restart hypercat.einarwh.no.service"
