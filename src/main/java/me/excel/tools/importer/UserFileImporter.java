package me.excel.tools.importer;

import me.excel.tools.factory.ModelFactory;
import me.excel.tools.processor.DataProcessor;
import me.excel.tools.setter.FieldValueSetter;

/**
 * file importer
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileImporter {

  void process(DataProcessor dataProcessor);

  /**
   * @param setters
   * @see FieldValueSetter
   */
  void addCellValueSetter(FieldValueSetter... setters);

  void setModelFactory(ModelFactory modelFactory);
}
