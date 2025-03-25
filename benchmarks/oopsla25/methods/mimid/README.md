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

> If you are on an Apple Silicon machine, you might need to first enable X86 emulation in VirtualBox by issuing the following command: `VBoxManage setextradata global "VBoxInternal2/EnableX86OnArm" 1`

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

Note that Mimid requires a golden grammar for each evaluation subject to derive the positive sample inputs required by the approach. By default, the Mimid artifact generates up to 100 sample inputs from each golden grammar (in Fuzzing Book format) as part of the mining process.

## Bonus: Run `urlparse.py` in Mimid Python notebook

As we do not have a C version of the `urlparse` benchmark, you need to run it from the Jupyter notebook included with the Mimid artifact. To reproduce the results with the correct golden grammar, you first need to edit the notebook, located at `/home/vagrant/mimid/src/PymimiBook.ipynb`:

1. Under section 2.2.1.1, change the `urlparse_golden` variable to point to the correct grammar, pasted from this project's `benchmarks/oopsla25/subjects/urlparse.grammar` file. Be sure to uppercase the `"<start>"` key to `"<START>"`.
2. In the first cell of section 2.2.1.2, replace  `canonical(urlparse_golden)` with `urlparse_golden`.
3. In section 2.1.1, in the definition of the `check_recall` function, replace `canonical(golden_grammar)` with `golden_grammar`.

You can now start the notebook as per the instructions in the artifact's README and execute all cells up to and including the last one in section 2.2.1.3. Should you experience an error, you might first want to try to re-run section 2.2.1.2 to generate new samples. To get the benchmark runtime, it suffices to execute only the last cell in section 2.3 and then the command `to_sec(Mimid_t)`.

## References

* Rahul Gopinath, Björn Mathis, and Andreas Zeller. 2020. Mining input grammars from dynamic control flow. ESEC/FSE 2020. <https://doi.org/10.1145/3368089.3409679>
