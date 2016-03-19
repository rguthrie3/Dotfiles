import zipfile
import shutil
import tempfile
import requests

from os import path


#--- Globals ----------------------------------------------
PLUGINS = """
ack.vim https://github.com/mileszs/ack.vim
bufexplorer https://github.com/corntrace/bufexplorer
ctrlp.vim https://github.com/kien/ctrlp.vim
goyo.vim https://github.com/junegunn/goyo.vim
LaTeX-Box https://github.com/vim-scripts/LaTeX-Box
neomake https://github.com/benekastah/neomake
nerdtree https://github.com/scrooloose/nerdtree
open_file_under_cursor.vim https://github.com/amix/open_file_under_cursor.vim
taglist.vim https://github.com/vim-scripts/taglist.vim
tlib https://github.com/vim-scripts/tlib
ultisnips https://github.com/SirVer/ultisnips
vim-addon-mw-utils https://github.com/MarcWeber/vim-addon-mw-utils
vim-airline https://github.com/bling/vim-airline
vim-colortuner https://github.com/zefei/vim-colortuner
vim-commentary https://github.com/tpope/vim-commentary
vim-dispatch https://github.com/tpope/vim-dispatch
vim-easymotion https://github.com/easymotion/vim-easymotion
vim-endwise https://github.com/tpope/vim-endwise
vim-expand-region https://github.com/terryma/vim-expand-region
vim-fugitive https://github.com/tpope/vim-fugitive
vim-indent-object https://github.com/michaeljsmith/vim-indent-object
vim-multiple-cursors https://github.com/terryma/vim-multiple-cursors
vim-obsession https://github.com/tpope/vim-obsession
vim-qf https://github.com/romainl/vim-qf
vim-repeat https://github.com/tpope/vim-repeat
vim-snippets https://github.com/honza/vim-snippets
vim-surround https://github.com/tpope/vim-surround
vim-tmux-navigator https://github.com/christoomey/vim-tmux-navigator
YCM-Generator https://github.com/rdnetto/YCM-Generator
""".strip()

GITHUB_ZIP = '%s/archive/master.zip'

SOURCE_DIR = path.join(path.dirname(__file__), 'bundle')


def download_extract_replace(plugin_name, zip_path, temp_dir, source_dir):
    temp_zip_path = path.join(temp_dir, plugin_name)

    # Download and extract file in temp dir
    req = requests.get(zip_path)
    open(temp_zip_path, 'wb').write(req.content)

    zip_f = zipfile.ZipFile(temp_zip_path)
    zip_f.extractall(temp_dir)

    plugin_temp_path = path.join(temp_dir,
                                 path.join(temp_dir, '%s-master' % plugin_name))

    # Remove the current plugin and replace it with the extracted
    plugin_dest_path = path.join(source_dir, plugin_name)

    try:
        shutil.rmtree(plugin_dest_path)
    except OSError:
        pass

    shutil.move(plugin_temp_path, plugin_dest_path)

    print('Updated {0}'.format(plugin_name))


if __name__ == '__main__':
    temp_directory = tempfile.mkdtemp()

    try:
        for line in PLUGINS.splitlines():
            name, github_url = line.split(' ')
            zip_path = GITHUB_ZIP % github_url
            download_extract_replace(name, zip_path,
                                     temp_directory, SOURCE_DIR)
    finally:
        shutil.rmtree(temp_directory)
