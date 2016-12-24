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

  /**
   * @param dataProcessor processor
   * @see DataProcessor
   */
  void process(DataProcessor dataProcessor);

  /**
   * @param setters field value setter
   * @see FieldValueSetter
   */
  void addCellValueSetter(FieldValueSetter... setters);

  /**
   * @param modelFactory model factory
   * @see ModelFactory
   */
  void setModelFactory(ModelFactory modelFactory);
}
