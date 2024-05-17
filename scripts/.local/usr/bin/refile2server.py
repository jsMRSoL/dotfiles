#!/home/simon/.virtualenvs/serverrefiler/bin/python
# -*- coding: utf-8 -*-
from paramiko import SSHClient
import os
import sys
import subprocess
import logging

logging.basicConfig(level=logging.DEBUG)
logging.disable(logging.CRITICAL)


def get_file_count(id):
    # Connect
    client = SSHClient()
    client.load_system_host_keys()
    client.connect('ant', username='simon')

    # Check if folder exists
    stdin, stdout, stderr = client.exec_command(f'ls ~/vcol/{id}')
    if stdout.channel.recv_exit_status() != 0:
        print("Could not get info from server. Exiting...")
        client.close()
        sys.exit(0)

    if 'No such file or directory' in stderr.read().decode("utf8"):
        stdin, stdout, stderr = client.exec_command(
            f'mkdir ~/vcol/{id}/thumbs -p')

        if stdout.channel.recv_exit_status() != 0:
            print(f'Folder {id} does not exist in ~/vcol & cannot be created')
            client.close()
            sys.exit(0)

        client.close()
        return 0

    # Check for  any filenames with underscore
    cmd = f'find ~/vcol/{id} -maxdepth 1 -type f -name *_* | wc -l'
    stdin, stdout, stderr = client.exec_command(cmd)
    with stdin, stdout, stderr as stdin, stdout, stderr:
        no_files = stdout.read().decode("utf8")
        if int(no_files) == 0:
            client.close()
            return 0

    # Get last filename
    cmd = f'find ~/vcol/{id} -maxdepth 1 -type f -name *_* | sort | tail -n 1'
    stdin, stdout, stderr = client.exec_command(cmd)
    with stdin, stdout, stderr as stdin, stdout, stderr:
        output = stdout.read().decode("utf8")

    # Close the client itself
    client.close()

    logging.debug(output)

    if output == '':
        output = 'dummy_000.jpg'

    try:
        num = output.split("_")[1]
        num = num.split(".")[0]
        return int(num)

    except IndexError as e:
        print("Filename was not in expected format.")
        print(f"Error: {e}")
        sys.exit(0)
    except ValueError as e:
        print("Server did not return an integer. Exiting...")
        print(f"Error: {e}")
        sys.exit(0)


def push2server(id, num):
    files = []
    currentDir = os.getcwd()
    for file in os.listdir(currentDir):
        filename = os.fsdecode(file)
        if os.path.isfile(filename):
            files.append(filename)

    no_files = len(files)
    print(f'About to transfer {no_files} files:')
    filelist = '\n'.join(files)
    print(filelist)

    counter = 0
    copied_files = []
    for name in files:
        basename, ext = os.path.splitext(name)
        num += 1
        counter += 1
        print(f"Copying file {counter} of {no_files}...")
        print(f"....> {name}")
        scp_cmd = f'scp "{name}" simon@ant:~/vcol/{id}/{id}_{num:0>3}{ext}'
        scp_output = subprocess.run(scp_cmd,
                                    stdout=subprocess.DEVNULL,
                                    shell=True)
        if scp_output.returncode == 0:
            print(f'File {counter} successfully copied!')
            copied_files.append(name)
        else:
            print(f'{name} was NOT copied!')

    list = ' '.join(f'"{f}"' for f in files)
    subprocess.run(f'rm {list}', stdout=subprocess.DEVNULL, shell=True)


def make_thumbs(id):
    print("Generating thumbnails...")
    client = SSHClient()
    client.load_system_host_keys()
    client.connect('ant', username='simon')

    # Check if folder exists
    path = '/home/simon/.local/usr/bin'
    p1 = f'cd ~/vcol/{id}'
    p1a = '[[ -d thumbs ]] || mkdir thumbs'
    p2 = f'{path}/dirdiff.py . thumbs -thumb.jpg'
    p3 = f'xargs {path}/squarethumb.sh'
    cmd = f'{p1}; {p1a}; {p2} | {p3}'
    stdin, stdout, stderr = client.exec_command(cmd)
    # output = stdout.read().decode("utf8")
    # err = stderr.read().decode("utf8")
    # print(f'Output: {output}')
    # print(f'Error: {err}')
    if stdout.channel.recv_exit_status() != 0:
        print('diffdir.py encountered an error.\nThis command failed:')
        print(f'{cmd}')
        sys.exit(0)

    # Close the client itself
    client.close()


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Please supply a folder name, e.g. BL")
        sys.exit(0)

    id = sys.argv[-1]
    num = get_file_count(id)
    push2server(id, num)
    make_thumbs(id)
