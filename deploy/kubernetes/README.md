# Deploying PolarisProfiler on Kubernetes

The profiler runs as a single-replica HTTP service. Other components
(schedulers, controllers) call it at `http://polaris-profiler` in-cluster:

| Endpoint | Method | Purpose |
|---|---|---|
| `/predict` | POST | metadata -> profile + resource estimates (read-only) |
| `/observations` | POST | completed workload -> error record, feeds the loop |
| `/profiles` | GET | per-profile size + current estimates |
| `/info` | GET | version, estimator, loop counters |
| `/healthz`, `/readyz` | GET | liveness / readiness probes |
| `/metrics` | GET | Prometheus metrics (scrape annotations are set) |

## 1. Build and load the image

```bash
docker build -t polaris-profiler .        # same as `make build` (tag :latest)
# kind:      kind load docker-image polaris-profiler:latest
# minikube:  minikube image load polaris-profiler:latest
# else:      docker push <registry>/polaris-profiler:<tag> and set
#            `images:` newName/newTag in kustomization.yaml
```

## 2. Provision the model and data volumes

The image contains no artifacts. The Deployment mounts two PVCs:

- `polaris-profiler-models`: `hdbscan_300_power_transform_euclidean.pkl`,
  `xgboost_final_model.json`, `onehot_enc_train_data.pkl`
- `polaris-profiler-data`: `100_001_sampled_workload_data.csv`

Create the claims first, then copy the artifacts in with a helper pod:

```bash
kubectl apply -f pvc.yaml
kubectl run artifact-loader --image=busybox --restart=Never \
  --overrides='{"spec":{"containers":[{"name":"artifact-loader","image":"busybox","command":["sleep","3600"],"volumeMounts":[{"name":"models","mountPath":"/models"},{"name":"data","mountPath":"/data"}]}],"volumes":[{"name":"models","persistentVolumeClaim":{"claimName":"polaris-profiler-models"}},{"name":"data","persistentVolumeClaim":{"claimName":"polaris-profiler-data"}}]}}'
kubectl cp ml_data-profiling/experiments/hdbscan_300_power_transform_euclidean.pkl artifact-loader:/models/
kubectl cp ml_data-profiling/experiments/xgboost_final_model.json                  artifact-loader:/models/
kubectl cp ml_data-profiling/experiments/onehot_enc_train_data.pkl                 artifact-loader:/models/
kubectl cp ml_data-profiling/experiments/100_001_sampled_workload_data.csv         artifact-loader:/data/
kubectl delete pod artifact-loader
```

(In a real cluster, prefer an initContainer pulling from object storage.)

## 3. Deploy

```bash
kubectl apply -k deploy/kubernetes/
kubectl rollout status deployment/polaris-profiler
```

## 4. Smoke-test

```bash
kubectl port-forward svc/polaris-profiler 8080:80 &
curl localhost:8080/readyz
curl -X POST localhost:8080/predict -H 'Content-Type: application/json' \
  -d '{"job_name":"j1","user":"u1","task_name":"t1","group":"g1","workload":"w1"}'
```

## Scaling caveat

The feedback loop's state (reference dataset, violation counters, ACQUIRES
history) is in-process and mutates on every `/observations` call. With more
than one replica, each replica drifts independently and predictions become
inconsistent. Keep `replicas: 1` (the manifest also uses `strategy: Recreate`)
until the profile store is externalized (e.g. a shared database or a
leader-elected writer). Read-heavy deployments can point `/predict`-only
clients at additional replicas that never receive observations.
