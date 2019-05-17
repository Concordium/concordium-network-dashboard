
# Install deps and build
NODE_ENV=development npm i

NODE_ENV=production npm install
NODE_ENV=production npm run build

# Create the image
docker build -t concordium-visualizations .

# Boot the image
docker run -it --rm -p 8080:80 --name concordium-visualizations-run concordium-visualizations
