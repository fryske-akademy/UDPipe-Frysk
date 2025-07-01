# UDPipe-Frysk
Software for lemmatizing, PoS tagging and dependency parsing of Frisian texts. The lemmatizer/tagger/parser are trained on Frisian text that is annotated according to the guidelines of Universal Dependencies version 2.

Follow the instructions below to build the Docker image and launch the container.

### 1. Clone the Repo

```
git clone https://github.com/fryske-akademy/UDPipe-Frysk.git
cd UDPipe-Frysk
```

### 2. Build the Docker Image

```
docker build -t udpipe-frysk .
```

### 3. Run the Container

```
docker run -p 3838:3838 udpipe-frysk
```

### 4. View in Browser

Open:
http://localhost:3838
