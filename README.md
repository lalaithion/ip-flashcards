# IP Generator

This project was designed to help a friend study for the CCNA; they kept asking
me to make up IP Addresses so they could tell me minutiae about them.

One of my mottos is "don't do anything boring more than once, if you can get a
computer to do it for you". So, naturally, I wrote a short Haskell program to
generate IPs for them. Of course, once you've written a program, people feel
like you owe them feature requests, so they also asked for a program to answer
said minutiae about the IPs.

In the end, this became a place for me to try and write a small Haskell program
of good enough quality to be shipped.


| Goal | Status |
-----------------
| Full unit tests covering the library | Done |
| Full integration tests covering the binaries | TODO |
| CI that verifies that tests pass and code is formatted | TODO |
| Installation via package manager (brew, apt, yum, nix) | TODO |

## How to install manually

If you have https://docs.haskellstack.org/en/stable/README/ installed, then the
only step you have to do after cloning this repository is run `stack build`,
and the two binaries will be built. You'll have to manually copy the binaries
out of the directory they were built into, and those are listed in the output.

## Examples

### ipgenerate

```
$ stack exec ipgenerate-exe -- --help
ip-generate is a command line tool for generating ip addresses according
to a variety of command line flags. It is a companion tool to ip-answer
and in general a line of output from ip-generate is a valid set of
arguments for ip-answer.

usage:        ip-generate [--private] [--cidr|--binary] [--class {A|B|C}] --number NUMBER

--private -p  Indicates that the IPs generated should be private

--cidr    -c  Indicates that the IPs should be displayed in cidr notation
--binary  -b  Indicates that the IPs should be displayed in mask notation
              If neither of the above are specified, a mix of the two will
              be generated

--class   -s  Indicates which class of IP should be generated. If not specified,
              both classes will be generated.

--number  -n  Followed by the number of IPs to generate. Required.

If two contradictory arguemnts are passed in, the one occuring later in the
list overrides the former.
```

```
$ stack exec ipgenerator-exe -- --number 3
148.196.53.122 /19
220.60.13.248 255.255.255.248
183.192.147.223 /24
```

```
$ stack exec ipgenerator-exe -- --number 3 -p --class A
10.124.46.189 /29
10.94.224.192 255.255.192.0
10.227.247.45 255.255.128.0
```

```
$ stack exec ipgenerator-exe -- --number 3 -pcs B
172.17.198.49 /22
172.28.176.212 /29
172.17.87.223 /18
```

### ipanswer

```
$ stack exec ipanswer-exe -- --help
ip-answer is a command line tool for answering information about
ip addresses with subnet masks. It is a companion tool to ip-generate
and in general a line of output from ip-generate is a valid set of
arguments for ip-answer.

usage:     ip-answer IP-ADDRESS MASK

IP-ADDRESS This is an IP address written as four numbers between 0 and 255
           separated by periods, such as 127.0.0.1 or 8.8.8.8

MASK       This is a mask either written in Cidr notation or as a
           bitmask. Cidr notation would be /8 or /21 and a bitmask
           would be 255.0.0.0 or 255.255.248.0

Common errors:
  * The IP-ADDRESS and the MASK must be separatec by a space.
    10.6.0.1/13 is not a valid argument.
```

```
$ stack exec ipanswer-exe -- 24.25.26.17 /18
Other format of mask: 255.255.192.0
Number of hosts: 16382
Number of subnets: "1024"
Subnet Address: 24.25.0.0
Broadcast Address: 24.25.63.255
Range of IP Addresses: 24.25.0.1-24.25.63.254
Wildcard mask: 0.0.63.255
```

```
stack exec ipanswer-exe -- 10.22.25.166 255.248.0.0
Other format of mask: /13
Number of hosts: 524286
Number of subnets: "32"
Subnet Address: 10.16.0.0
Broadcast Address: 10.23.255.255
Range of IP Addresses: 10.16.0.1-10.23.255.254
Wildcard mask: 0.7.255.255
```
