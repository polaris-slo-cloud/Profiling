.PHONY: build build-cli test api run plots notebook shell k8s-apply clean dirs

IMAGE ?= polaris-profiler

build:            ## build the HTTP service image (default docker target)
	docker build --target service -t $(IMAGE) .

build-cli:        ## build the CLI image
	docker build --target runtime -t $(IMAGE)-cli .

test:             ## run the unit tests inside the container build
	docker build --target test -t $(IMAGE)-test .

api: build        ## start the HTTP service on http://localhost:8080
	docker compose up -d api

run: build-cli dirs  ## run the batch feedback loop with mounted artifacts
	docker compose run --rm profiler --estimator q05

plots: build-cli dirs ## generate skewness + RMSE figures
	docker compose run --rm plots

notebook: dirs    ## start JupyterLab on http://localhost:8888
	docker compose up notebook

shell: build-cli  ## interactive shell inside the CLI image
	docker compose run --rm --entrypoint bash profiler

k8s-apply:        ## deploy to the current kubectl context
	kubectl apply -k deploy/kubernetes/

dirs:             ## host dirs for writable bind mounts (root-owned if Docker creates them)
	mkdir -p results figures notebooks

clean:            ## remove built images
	docker rmi -f $(IMAGE) $(IMAGE)-cli $(IMAGE)-test $(IMAGE)-notebook 2>/dev/null || true
