package java.excel.engine.importer.template;

import java.excel.engine.exception.ExcelReadException;
import java.excel.engine.importer.processor.DefaultObjectProcessorEngine;
import java.excel.engine.importer.processor.ObjectProcessorEngine;
import java.excel.engine.importer.validator.DefaultExcelValidatorEngine;
import java.excel.engine.importer.validator.ExcelValidatorEngine;
import java.excel.engine.model.excel.Workbook;
import java.excel.engine.model.ext.SheetTemplate;
import java.excel.engine.util.ExcelReadHelper;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Created by hanwen on 15-12-16.
 */
public class DefaultExcelTemplateEngine implements ExcelTemplateEngine {

  private Workbook workbook;

  private ExcelValidatorEngine excelValidatorEngine;

  private ObjectProcessorEngine objectProcessorEngine;

  private Map<Integer, SheetTemplate> sheetIndex2sheetTemplate = new HashMap<>();

  public DefaultExcelTemplateEngine(File excel) throws IOException {
    this(new FileInputStream(excel));
  }

  public DefaultExcelTemplateEngine(InputStream inputStream) throws IOException {

    workbook = ExcelReadHelper.read(inputStream, sheetIndex2sheetTemplate);

    objectProcessorEngine = new DefaultObjectProcessorEngine(workbook);

    excelValidatorEngine = new DefaultExcelValidatorEngine(workbook);
  }

  @Override
  public void addSheetTemplate(SheetTemplate... sheetTemplates) {
    if (sheetTemplates == null) {
      return;
    }

    for (SheetTemplate sheetTemplate : sheetTemplates) {
      sheetIndex2sheetTemplate.put(sheetTemplate.getSheetIndex(), sheetTemplate);
    }
  }

  @Override
  public ExcelValidatorEngine getExcelValidatorEngine() {
    return excelValidatorEngine;
  }

  @Override
  public ObjectProcessorEngine getObjectProcessorEngine() {
    return objectProcessorEngine;
  }

  @Override
  public Set<String> getDistinctValuesOfField(int sheetIndex, String field) {

    if (workbook == null) {
      throw new ExcelReadException("workbook is null");
    }

    return workbook.getSheet(sheetIndex).getDistinctValuesOfField(field);
  }

}