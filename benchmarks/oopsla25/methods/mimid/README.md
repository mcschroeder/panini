# Evaluation of Mimid on the Panini benchmark dataset

This is a step-by-step guide on how to evaluate the Mimid grammar mining system (Gopinath, Mathis, Zeller 2020) on the Panini benchmark dataset. The Mimid artifact runs inside a Vagrant box (<https://www.vagrantup.com>) that is allocated 10 GB RAM and uses port 8888 for communication with the host system.

## 1. Download Mimid artifact

Download the artifact from <https://doi.org/10.5281/zenodo.3876969> (about 2.6 GB).

```shell
curl -o mimid.box https://zenodo.org/records/3876969/files/mimid.box
```

To ensure the integrity of the download you can compare its MD5 checksum with the reference `431f6ded243e91dcd5077b00ae2aa9b3`.

## 2. Import Vagrant box and SSH into VM

```shell
vagrant box add mimid ./mimid.box
vagrant up
vagrant ssh
```

From here on out, all commands are executed inside the Mimid VM.

## 3. Patch Mimid

This patch fixes an issue where grammars for small languages (<1000 words) would lead to infinite loops during the generalization phase.

```console
vagrant@ubuntu1804:~$ patch -d /home/vagrant/mimid -p1 < /vagrant/patch.diff
```

## 4. Run evaluation

```console
vagrant@ubuntu1804:~$ /vagrant/run_eval.sh
```

This will take about 1-2 hours. The mined grammars will be in the `results` directory (outside the VM), which will also contain a `results.csv` with the collected run times and exit codes.

Note that Mimid requires a golden grammar for each evaluation subject to derive the positive sample inputs required by the approach. By default, the Mimid artifact generates up to 1.000 sample inputs from each golden grammar (in Fuzzing Book format) as part of the mining process.

## References

* Rahul Gopinath, Björn Mathis, and Andreas Zeller. 2020. Mining input grammars from dynamic control flow. ESEC/FSE 2020. <https://doi.org/10.1145/3368089.3409679>
