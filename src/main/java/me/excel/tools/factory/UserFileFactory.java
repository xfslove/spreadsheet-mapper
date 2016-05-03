package me.excel.tools.factory;

import me.excel.tools.extractor.CellValueExtractor;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * file factory
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileFactory {

  /**
   * 设置file中的titles (第一行)
   *
   * @param titles
   */
  void setTitles(String... titles);

  /**
   * 设置file中的fields (第二行)
   *
   * @param fields
   */
  void setFields(String... fields);

  /**
   * 设置自定义的属性提取器
   *
   * @param cellValueExtractors
   */
  void addValueExtractors(CellValueExtractor... cellValueExtractors);

  /**
   * 设置file中的data (从第四行开始)
   *
   * @param data
   */
  void setData(List data);

  /**
   * 生成文件
   *
   * @param file
   * @throws IOException
   */
  void generate(File file) throws IOException;

  void generate(File excel, boolean createTitles, boolean createFields, boolean createPrompts) throws IOException;
}
