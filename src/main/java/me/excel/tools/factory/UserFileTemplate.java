package me.excel.tools.factory;

import me.excel.tools.processor.ObjectProcessor;
import me.excel.tools.validator.ExcelValidator;

import java.util.Set;

/**
 * import template factory
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileTemplate {

  /**
   * @return userFileValidator
   * @see ExcelValidator
   */
  ExcelValidator getExcelValidator();

  /**
   * @return objectProcessor
   * @see ObjectProcessor
   */
  ObjectProcessor getObjectProcessor();

  /**
   * @param field field
   * @return distinct values of supplied field
   */
  Set<String> getDistinctValuesOfField(String field);
}
