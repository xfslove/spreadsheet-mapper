package me.excel.tools.importer;

import me.excel.tools.factory.ModelFactory;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.processor.DataProcessor;
import me.excel.tools.transfer.ExcelFileTransfer;
import me.excel.tools.setter.CellValueSetter;
import me.excel.tools.setter.DefaultValueSetter;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * excel 文件导入器
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileImporter implements UserFileImporter {

  protected ExcelFileTransfer excelFileTransfer;

  protected ModelFactory modelFactory;

  protected List<CellValueSetter> cellValueSetters = new ArrayList<>();

  protected DefaultValueSetter defaultValueSetter = new DefaultValueSetter();

  public ExcelFileImporter(ExcelFileTransfer excelFileTransfer) {
    this.excelFileTransfer = excelFileTransfer;
  }

  @Override
  public void process(File excel, DataProcessor dataProcessor) throws IOException {

    if (excel == null) {
      throw new IllegalArgumentException("file is null");
    }
    if (dataProcessor == null) {
      throw new IllegalArgumentException("dataProcessor is null");
    }

    FileInputStream inputStream = new FileInputStream(excel);

    ExcelSheet excelSheet = excelFileTransfer.transfer(true, inputStream).getFirstSheet();

    List models = new ArrayList<>();
    excelSheet.getDataRows().forEach(row -> {
      Object model = modelFactory.create(row);

      dataProcessor.preProcessing(model);

      defaultValueSetter.set(model, row.getCells());

      for (ExcelCell excelCell : row.getCells()) {

        cellValueSetters.stream()
            .filter(customValueSetter -> customValueSetter.matches(excelCell))
            .forEach(customValueSetter -> customValueSetter.set(model, excelCell));
      }

      dataProcessor.postProcessing(model);

      models.add(model);
    });

    dataProcessor.handle(models);
  }

  @Override
  public void addCellValueSetter(CellValueSetter... setters) {
    if (setters == null) {
      return;
    }

    for (CellValueSetter setter : setters) {
      this.cellValueSetters.add(setter);
    }
  }

  @Override
  public void setModelFactory(ModelFactory modelFactory) {
    this.modelFactory = modelFactory;
  }
}
