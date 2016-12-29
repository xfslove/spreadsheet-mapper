package excel.engine.importer.template;

import excel.engine.importer.processor.ObjectProcessorEngine;
import excel.engine.importer.validator.ExcelValidatorEngine;
import java.util.Set;

/**
 * excel import template engine
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ExcelTemplateEngine {

  /**
   * @return excelValidatorEngine
   * @see ExcelValidatorEngine
   */
  ExcelValidatorEngine getExcelValidatorEngine();

  /**
   * @return objectProcessorEngine
   * @see ObjectProcessorEngine
   */
  ObjectProcessorEngine getObjectProcessorEngine();

  /**
   * @param sheetIndex which sheet
   * @param field      field
   * @return distinct values of supplied field
   */
  Set<String> getDistinctValuesOfField(int sheetIndex, String field);
}
