#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from paramiko import SSHClient
import os
import sys
import subprocess


def get_file_count(id):
    # Connect
    client = SSHClient()
    client.load_system_host_keys()
    client.connect('ant', username='simon')

    # Check if folder exists
    stdin, stdout, stderr = client.exec_command(f'ls ~/vcol/{id}')
    if 'No such file or directory' in stderr.read().decode("utf8"):
        stdin, stdout, stderr = client.exec_command(f'mkdir ~/vcol/{id}')
        if stdout.channel.recv_exit_status() != 0:
            print(f'Folder {id} does not exist in ~/vcol & cannot be created')
            sys.exit(0)
        return 0

    # Get last filename
    stdin, stdout, stderr = client.exec_command(f'ls ~/vcol/{id} | tail -n 1')
    with stdin, stdout, stderr as stdin, stdout, stderr:
        output = stdout.read().decode("utf8")

        # Get return code from command (0 is default for success)
        if stdout.channel.recv_exit_status() != 0:
            print("Could not get info from server. Exiting...")
            sys.exit(0)
    # Close the client itself
    client.close()

    num = output.split("_")[1]
    num = num.split(".")[0]

    try:
        return int(num)
    except ValueError as e:
        print("Server did not return an integer. Exiting...")
        print(f"Error: {e}")
        sys.exit(0)


def push2server(id, num):
    files = []
    currentDir = os.getcwd()
    for file in os.listdir(currentDir):
        filename = os.fsdecode(file)
        files.append(filename)

    copied_files = []
    for name in files:
        basename, ext = os.path.splitext(name)
        num += 1
        scp_cmd = f'scp "{name}" simon@ant:~/vcol/{id}/{id}_{num:0>3}{ext}'
        scp_output = subprocess.run(
            scp_cmd, stdout=subprocess.DEVNULL, shell=True)
        if scp_output.returncode == 0:
            print(f'{name} successfully copied!')
            copied_files.append(name)
        else:
            print(f'{name} was NOT copied!')

    list = ' '.join(f'"{f}"' for f in files)
    subprocess.run(f'rm {list}', stdout=subprocess.DEVNULL, shell=True)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please supply a folder name, e.g. BL")
        sys.exit(0)

    id = sys.argv[-1]
    num = get_file_count(id)
    push2server(id, num)
