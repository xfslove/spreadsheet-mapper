package me.excel.tools.importer;

import me.excel.tools.factory.ModelFactory;
import me.excel.tools.processor.DataProcessor;
import me.excel.tools.setter.CellValueSetter;

/**
 * file importer
 *
 * Created by hanwen on 15-12-16.
 */
public interface UserFileImporter {

  void process(DataProcessor dataProcessor);

  /**
   * 设置自定义的value setter
   *
   * @param setters
   */
  void addCellValueSetter(CellValueSetter... setters);

  void setModelFactory(ModelFactory modelFactory);
}
