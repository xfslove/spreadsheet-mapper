package me.excel.tools.factory;

import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.validator.UserFileValidator;

import java.util.Set;

/**
 * <pre>
 * import template factory
 *
 * template format:
 *
 * first row  : titles
 * --------------------
 * second row : fields
 * --------------------
 * third row  : prompts
 * --------------------
 * data row ...
 * </pre>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileTemplate {

  /**
   * @return userFileValidator
   * @see UserFileValidator
   */
  UserFileValidator getUserFileValidator();

  /**
   * @return userFileImporter
   * @see UserFileImporter
   */
  UserFileImporter getUserFileImporter();

  /**
   * @param field field
   * @return distinct values of supplied field
   * @see me.excel.tools.model.excel.ExcelSheet#getDistinctCellValuesByField(String)
   */
  Set<String> getCellValuesOfField(String field);
}
