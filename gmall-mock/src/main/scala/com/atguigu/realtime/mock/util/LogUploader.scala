package com.atguigu.realtime.mock.util

import java.io.OutputStream
import java.net.{HttpURLConnection, URL}


object LogUploader {
    /*发送日志*/
    def sendLog(log: String): Unit = {
        try {
            // 1. 日志服务器的地址
            val logUrl = new URL("http://hadoop102:8081/log") //测试的时候在window下写代码
            // 2. 得到一个 HttpURLConnection
            val conn: HttpURLConnection = logUrl.openConnection().asInstanceOf[HttpURLConnection]
            // 3. 设置请求方法(上传数据一般使用 post 请求)
            conn.setRequestMethod("POST")
            // 4. 用来供server进行时钟校对的
            conn.setRequestProperty("clientTime", System.currentTimeMillis + "")
            // 5. 允许上传数据
            conn.setDoOutput(true)  //通过流的方式上传.打开允许
            // 6. 设置请求的头信息, post 请求必须这样设置
            conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded")//POST 请求头必须加
            // 7. 获取上传用的输出流
            val out: OutputStream = conn.getOutputStream
            // 8. 写出数据  log=.... 键值对的方式写出
            out.write(("log=" + log).getBytes())
            // 9. flush
            out.flush()
            // 10. 关闭资源
            out.close()
            // 11. 获取响应码.  (或者获取响应信息也行, 否则不会发送请求到服务器)  200 ok  404 资源没有找到  500 服务器故障
            val code: Int = conn.getResponseCode
            println(code)
        } catch {
            case e: Exception => e.printStackTrace()
        }
    }
}

