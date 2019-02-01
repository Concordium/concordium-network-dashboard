
# Install deps and build
npm i
npm run build

# Create the image
docker build -t concordium-dashboard .

# Boot the image
docker run -it --rm -p 8080:80 --name dashboard-run concordium-dashboard
