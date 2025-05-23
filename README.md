# UDPipe-Frysk
Software for a Universal Dependencies-based lemmatizer, POS-tagger, and annotator for Frisian.
To build the Docker image and launch the container, follow the steps below.

### 1. Clone the Repo

```
git clone https://github.com/heeringa0/UDPipe-Frysk.git
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
