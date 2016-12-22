package me.excel.tools.factory;

import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.validator.UserFileValidator;

import java.util.Set;

/**
 * import template factory
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileTemplate {

  /**
   * @param field
   * @return
   * @see me.excel.tools.model.excel.ExcelSheet#getDistinctCellValuesOfField(String)
   */
  Set<String> getCellValuesOfField(String field);

  /**
   * @return
   * @see UserFileValidator
   */
  UserFileValidator getUserFileValidator();

  /**
   * @return
   * @see UserFileImporter
   */
  UserFileImporter getUserFileImporter();
}
