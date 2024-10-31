from flask import Flask, request, render_template, send_file, url_for
import os
import subprocess
from werkzeug.utils import secure_filename

app = Flask(__name__)
app.config['UPLOAD_FOLDER'] = 'uploads/'
app.config['OUTPUT_FOLDER'] = 'static/outputs/'

# 确保文件夹存在
os.makedirs(app.config['UPLOAD_FOLDER'], exist_ok=True)
os.makedirs(app.config['OUTPUT_FOLDER'], exist_ok=True)

# 首页，提供文件上传表单
@app.route('/', methods=['GET', 'POST'])
def index():
    if request.method == 'POST':
        # 检查是否有上传文件
        if 'iat4' not in request.files or 'iat7' not in request.files:
            return "请同时上传 'iat4' 和 'iat7' 文件夹内容", 400

        # 保存上传的文件
        iat4_files = request.files.getlist('iat4')
        iat7_files = request.files.getlist('iat7')

        # 创建文件夹保存上传内容
        iat4_dir = os.path.join(app.config['UPLOAD_FOLDER'], 'iat4')
        iat7_dir = os.path.join(app.config['UPLOAD_FOLDER'], 'iat7')
        os.makedirs(iat4_dir, exist_ok=True)
        os.makedirs(iat7_dir, exist_ok=True)

        for file in iat4_files:
            file.save(os.path.join(iat4_dir, secure_filename(file.filename)))
        for file in iat7_files:
            file.save(os.path.join(iat7_dir, secure_filename(file.filename)))

        # 调用 R 脚本处理数据
        subprocess.run(["Rscript", "process_iat_data.R", iat4_dir, iat7_dir, app.config['OUTPUT_FOLDER']])

        # 获取图像和数据文件的 URL
        merged_data_url = url_for('download_file', filename='merged_data.csv')
        effect_merge_url = url_for('static', filename='outputs/IATeffect_merge.svg')
        effect_url = url_for('static', filename='outputs/IATeffect.svg')

        # 返回页面显示结果
        return render_template('index.html', 
                               merged_data_url=merged_data_url, 
                               effect_merge_url=effect_merge_url, 
                               effect_url=effect_url)

    return render_template('index.html')

# 文件下载路由
@app.route('/download/<filename>')
def download_file(filename):
    path = os.path.join(app.config['OUTPUT_FOLDER'], filename)
    return send_file(path, as_attachment=True)

if __name__ == '__main__':
    app.run(debug=True)
