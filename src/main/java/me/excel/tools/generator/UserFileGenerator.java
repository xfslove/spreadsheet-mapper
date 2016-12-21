package me.excel.tools.generator;

import me.excel.tools.extractor.CellValueExtractor;
import me.excel.tools.prompter.CellPrompter;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

/**
 * file generator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileGenerator {

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
   * 设置field的提示
   *
   * @param cellPrompters
   */
  void addCellPrompters(CellPrompter... cellPrompters);

  /**
   * 设置file中的data (从第四行开始)
   *
   * @param data
   */
  void setData(List data);

  /**
   * 生成文件
   *
   * @throws IOException
   */
  void generate(OutputStream outputStream) throws IOException;

  void generate(OutputStream outputStream, boolean createTitles, boolean createFields, boolean createPrompts) throws IOException;
}
