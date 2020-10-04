---
title: Minikubeを使って、kubernetesを構築する。
published: 2020-08-06 23:11:42
tags: kubernetes, minikube
---

# はじめに
**この記事は、はてなブログで公開していた記事を持ってきたものです（一部リンク等を修正しています）**

# 追記
`libvirt` グループに所属させないと、minikubeでkvm仮想マシンを作成する時エラーになるので、手順を追記しました。

# 目的
kubernetesを全く触ったことがなく、どんなものか知りたかったので、ちょっと触ってみます。
もともとはEKSで試そうとしましたが、0.20$/hと試してみるには高かったので、ローカル環境でMinikubeを使って構築してみます。

試した環境はこちら。

#TEASER#

```
$ cat /etc/centos-release
CentOS Linux release 7.7.1908 (Core)
```

インストール自体は、[公式サイト](https://kubernetes.io/docs/tasks/tools/install-minikube/)を参考に実施していきます。


まず、公式サイトを参考にして、仮想化がサポートされているか確認しておきます。

```
$ grep -E --color 'vmx|svm' /proc/cpuinfo 
flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx rdtscp lm constant_tsc arch_perfmon pebs bts rep_good nopl xtopology nonstop_tsc aperfmperf eagerfpu pni pclmulqdq dtes64 monitor ds_cpl vmx smx est tm2 ssse3 cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic popcnt tsc_deadline_timer aes xsave avx f16c rdrand lahf_lm epb ssbd ibrs ibpb stibp tpr_shadow vnmi flexpriority ept vpid fsgsbase smep erms xsaveopt dtherm ida arat pln pts md_clear spec_ctrl intel_stibp flush_l1d
flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx rdtscp lm constant_tsc arch_perfmon pebs bts rep_good nopl xtopology nonstop_tsc aperfmperf eagerfpu pni pclmulqdq dtes64 monitor ds_cpl vmx smx est tm2 ssse3 cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic popcnt tsc_deadline_timer aes xsave avx f16c rdrand lahf_lm epb ssbd ibrs ibpb stibp tpr_shadow vnmi flexpriority ept vpid fsgsbase smep erms xsaveopt dtherm ida arat pln pts md_clear spec_ctrl intel_stibp flush_l1d
flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx rdtscp lm constant_tsc arch_perfmon pebs bts rep_good nopl xtopology nonstop_tsc aperfmperf eagerfpu pni pclmulqdq dtes64 monitor ds_cpl vmx smx est tm2 ssse3 cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic popcnt tsc_deadline_timer aes xsave avx f16c rdrand lahf_lm epb ssbd ibrs ibpb stibp tpr_shadow vnmi flexpriority ept vpid fsgsbase smep erms xsaveopt dtherm ida arat pln pts md_clear spec_ctrl intel_stibp flush_l1d
flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx rdtscp lm constant_tsc arch_perfmon pebs bts rep_good nopl xtopology nonstop_tsc aperfmperf eagerfpu pni pclmulqdq dtes64 monitor ds_cpl vmx smx est tm2 ssse3 cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic popcnt tsc_deadline_timer aes xsave avx f16c rdrand lahf_lm epb ssbd ibrs ibpb stibp tpr_shadow vnmi flexpriority ept vpid fsgsbase smep erms xsaveopt dtherm ida arat pln pts md_clear spec_ctrl intel_stibp flush_l1d
```

# KVMのインストール
Minikubeはローカル環境の仮想マシン上に、Kubernetesをインストールします。そのため、ハイパーバイザをインストールする必要があります。
LinuxだとVirtualboxとKVMが使用できますが、今回はKVMを使ってみることにします。

[この辺り](https://minikube.sigs.k8s.io/docs/reference/drivers/kvm2/)を参考にして、KVMをインストールします。



```
$ sudo yum install qemu-kvm libvirt libvirt-python libguestfs-tools virt-install
$ sudo systemctl start libvirtd.service
$ sudo systemctl enable libvirtd.service
$ sudo usermod -aG libvirt $(whoami)
```

ちゃんとインストールできたか、確認します。
```
$ virt-host-validate 
  QEMU: 確認中 for hardware virtualization                                 : 成功
  QEMU: 確認中 if device /dev/kvm exists                                   : 成功
  QEMU: 確認中 if device /dev/kvm is accessible                            : 成功
  QEMU: 確認中 if device /dev/vhost-net exists                             : 成功
  QEMU: 確認中 if device /dev/net/tun exists                               : 成功
  QEMU: 確認中 for cgroup 'memory' controller support                      : 成功
  QEMU: 確認中 for cgroup 'memory' controller mount-point                  : 成功
  QEMU: 確認中 for cgroup 'cpu' controller support                         : 成功
  QEMU: 確認中 for cgroup 'cpu' controller mount-point                     : 成功
  QEMU: 確認中 for cgroup 'cpuacct' controller support                     : 成功
  QEMU: 確認中 for cgroup 'cpuacct' controller mount-point                 : 成功
  QEMU: 確認中 for cgroup 'cpuset' controller support                      : 成功
  QEMU: 確認中 for cgroup 'cpuset' controller mount-point                  : 成功
  QEMU: 確認中 for cgroup 'devices' controller support                     : 成功
  QEMU: 確認中 for cgroup 'devices' controller mount-point                 : 成功
  QEMU: 確認中 for cgroup 'blkio' controller support                       : 成功
  QEMU: 確認中 for cgroup 'blkio' controller mount-point                   : 成功
  QEMU: 確認中 for device assignment IOMMU support                         : 警告 (No ACPI DMAR table found, IOMMU either disabled in BIOS or not supported by this hardware platform)
   LXC: 確認中 Linux >= 2.6.26 向けです                                : 成功
   LXC: 確認中 for namespace ipc                                           : 成功
   LXC: 確認中 for namespace mnt                                           : 成功
   LXC: 確認中 for namespace pid                                           : 成功
   LXC: 確認中 for namespace uts                                           : 成功
   LXC: 確認中 for namespace net                                           : 成功
   LXC: 確認中 for namespace user                                          : 成功
   LXC: 確認中 for cgroup 'memory' controller support                      : 成功
   LXC: 確認中 for cgroup 'memory' controller mount-point                  : 成功
   LXC: 確認中 for cgroup 'cpu' controller support                         : 成功
   LXC: 確認中 for cgroup 'cpu' controller mount-point                     : 成功
   LXC: 確認中 for cgroup 'cpuacct' controller support                     : 成功
   LXC: 確認中 for cgroup 'cpuacct' controller mount-point                 : 成功
   LXC: 確認中 for cgroup 'cpuset' controller support                      : 成功
   LXC: 確認中 for cgroup 'cpuset' controller mount-point                  : 成功
   LXC: 確認中 for cgroup 'devices' controller support                     : 成功
   LXC: 確認中 for cgroup 'devices' controller mount-point                 : 成功
   LXC: 確認中 for cgroup 'blkio' controller support                       : 成功
   LXC: 確認中 for cgroup 'blkio' controller mount-point                   : 成功
   LXC: 確認中 if device /sys/fs/fuse/connections exists                   : 失敗 (Load the 'fuse' module to enable /proc/ overrides)
```
```
$ id
（結果は省略）
```

`/sys/fs/fuse/connections`がないよ、って怒られています。
書いてある通り、`fuse`モジュールをロードします。ついでに、起動時に自動でロードするように設定しておきます。

```
$ sudo modprobe fuse
$ sudo sh -c "echo fuse > /etc/modules-load.d/fuse.conf"
```

# kubectlおよびMinikubeのインストール
こちらも公式サイト通りに。

```
$ curl -LO https://storage.googleapis.com/kubernetes-release/release/`curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt`/bin/linux/amd64/kubectl
$ chmod +x ./kubectl
$ sudo mv ./kubectl /usr/local/bin/kubectl
```
```
$ curl -LO https://storage.googleapis.com/minikube/releases/latest/minikube-1.6.2.rpm && sudo rpm -ivh minikube-1.6.2.rpm
```

早速クラスタを作ってみます。
```
$ minikube start --vm-driver=kvm2
😄  minikube v1.6.2 on Centos 7.7.1908
✨  Selecting 'kvm2' driver from user configuration (alternates: [none])
💾  Downloading driver docker-machine-driver-kvm2:
    > docker-machine-driver-kvm2.sha256: 65 B / 65 B [-------] 100.00% ? p/s 0s
    > docker-machine-driver-kvm2: 13.86 MiB / 13.86 MiB  100.00% 2.95 MiB p/s 5
🔥  Creating kvm2 VM (CPUs=2, Memory=2000MB, Disk=20000MB) ...
🐳  Preparing Kubernetes v1.17.0 on Docker '19.03.5' ...
🚜  Pulling images ...
🚀  Launching Kubernetes ... 
⌛  Waiting for cluster to come online ...
🏄  Done! kubectl is now configured to use "minikube"
```

ちゃんと作れたか確認してみましょう。

```
$ minikube status
host: Running
kubelet: Running
apiserver: Running
kubeconfig: Configured
```

大丈夫そうですね。

すぐには不要なので、一旦停止しておきます。

```
$ minikube stop
✋  Stopping "minikube" in kvm2 ...
🛑  "minikube" stopped.
```

これで、kubernetesを試す準備ができました。

