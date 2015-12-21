package me.excel.tools.processor;

import me.excel.tools.factory.ModelFactory;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.utils.FieldValueSetter;

/**
 * Created by hanwen on 15-12-16.
 */
public interface DataProcessor {

  void handle(ExcelSheet excelSheet);

  void addFieldValueSetter(FieldValueSetter... setters);

  void setModelFactory(ModelFactory modelFactory);
}
