package me.excel.tools.factory;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * file factory
 *
 * Created by hanwen on 15-12-16.
 */
public interface UserFileFactory {

  /**
   * 设置file中的fields
   *
   * @param fields
   */
  void setFields(String... fields);

  /**
   * 设置file中的data
   *
   * @param datas
   */
  void setDatas(List datas);

  /**
   * 生成文件
   *
   * @param file
   * @throws IOException
   */
  void generate(File file) throws IOException;

}
