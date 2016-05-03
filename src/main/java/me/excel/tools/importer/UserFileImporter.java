package me.excel.tools.importer;

import me.excel.tools.factory.ModelFactory;
import me.excel.tools.processor.DataProcessor;
import me.excel.tools.setter.CellValueSetter;

import java.io.File;
import java.io.IOException;

/**
 * file importer
 *
 * Created by hanwen on 15-12-16.
 */
public interface UserFileImporter {

  void process(File file, DataProcessor dataProcessor) throws IOException;

  /**
   * 设置自定义的value setter
   *
   * @param setters
   */
  void addCellValueSetter(CellValueSetter... setters);

  void setModelFactory(ModelFactory modelFactory);
}
