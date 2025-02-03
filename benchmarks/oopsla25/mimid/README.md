# Evaluation of Mimid on the Panini benchmark dataset

This is a step-by-step guide on how to evaluate the Mimid grammar mining system (Gopinath, Mathis, Zeller 2020) on the Panini benchmark dataset. The Mimid artifact runs inside a Vagrant box (<https://www.vagrantup.com>) that is allocated 10 GB RAM and uses port 8888 for communication with the host system.

## 1. Download Mimid artifact

Download the artifact from <https://doi.org/10.5281/zenodo.3876969> (about 2.6 GB).

```shell
curl -o mimid.box https://zenodo.org/records/3876969/files/mimid.box
```

To ensure the integrity of the download you can compare its MD5 checksum with the reference `431f6ded243e91dcd5077b00ae2aa9b3`.

## 2. Prepare subjects

The `prepare_subjects.sh` script copies the C subjects from the Panini evaluation dataset into a folder synced with the VM and prepares the scaffolding around them using the stuff in the `template` folder. It also sets up a custom `run_eval.sh` script for running the Mimid evaluation on our subjects inside the VM.

```shell
./prepare_subjects.sh
```

## 3. Prepare golden grammars

Mimid requires a golden grammar for each evaluation subject, not only to compute recall, but also to derive the positive sample inputs required by their grammar mining approach.

The ground truth of the Panini evaluation dataset is in the form of regular expressions. Mimid requires grammars to be in a variation of the Fuzzing Book format (Zeller et al. 2024). The `prepare_grammars.sh` script uses the `regex` tool from our `regex-algebra` package to do the conversion, adds some scaffolding, and copies the grammars into a folder synced with the VM.

```shell
./prepare_grammars.sh
```

## 4. Import Vagrant box and SSH into VM

```shell
vagrant box add mimid ./mimid.box
vagrant init mimid
vagrant up
vagrant ssh
```

From here on out, all commands are executed inside the Mimid VM.

## 5. Patch Mimid

This patch fixes an issue where small grammars would lead to infinite loops during the generalization phase or when computing precision or recall.

```console
vagrant@ubuntu1804:~$ patch -d /home/vagrant/mimid -p1 < /vagrant/patch.diff
```

## 6. Run evaluation

This will take about 4 hours. The collected results will be in `results.csv` in this folder (outside the VM). The mined grammars can be found in the `/home/vagrant/mimid/Cmimid/build` directory (inside the VM).

```console
vagrant@ubuntu1804:~$ chmod +x /vagrant/subjects/run_eval.sh
vagrant@ubuntu1804:~$ /vagrant/subjects/run_eval.sh
```

> Note: Somewhat confusingly, Mimid's `make build/*.precision` command and the `src/check_precision.py` script actually computes *recall*, while `make build/*.fuzz` and the `src/fuzz.py` script computes *precision*.

## References

* Rahul Gopinath, Björn Mathis, and Andreas Zeller. 2020. Mining input grammars from dynamic control flow. ESEC/FSE 2020. <https://doi.org/10.1145/3368089.3409679>
