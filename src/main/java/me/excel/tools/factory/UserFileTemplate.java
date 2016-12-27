package me.excel.tools.factory;

import me.excel.tools.processor.SheetToObjectsProcessor;
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
   * @see SheetToObjectsProcessor
   */
  SheetToObjectsProcessor getSheetToObjectsProcessor();

  /**
   * @param field field
   * @return distinct values of supplied field
   */
  Set<String> getDistinctValuesOfField(String field);
}
