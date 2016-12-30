package excel.engine.w2o.processor;

import excel.engine.model.core.Cell;
import excel.engine.model.core.Row;
import excel.engine.model.core.Sheet;
import excel.engine.model.meta.FieldMeta;
import excel.engine.model.meta.SheetMeta;
import excel.engine.w2o.setter.BeanUtilValueSetter;
import excel.engine.w2o.setter.ValueSetter;
import excel.engine.w2o.setter.FieldValueSetter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * sheet to objects processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DefaultSheetProcessor implements SheetProcessor {

  private Sheet sheet;

  private SheetMeta sheetMeta;

  private ObjectFactory objectFactory;

  private SheetProcessorListener sheetProcessorListener = new NoopSheetProcessorListener();

  private Map<String, FieldValueSetter> key2fieldValueSetter = new HashMap<>();

  private ValueSetter defaultValueSetter = new BeanUtilValueSetter();

  @Override
  public SheetProcessor fieldValueSetter(FieldValueSetter... setters) {
    if (setters == null) {
      return this;
    }

    for (FieldValueSetter setter : setters) {
      key2fieldValueSetter.put(setter.getMatchField(), setter);
    }
    return this;
  }

  @Override
  public SheetProcessor sheetProcessorListener(SheetProcessorListener sheetProcessorListener) {
    this.sheetProcessorListener = sheetProcessorListener;
    return this;
  }

  @Override
  public SheetProcessor objectFactory(ObjectFactory objectFactory) {
    this.objectFactory = objectFactory;
    return this;
  }

  @Override
  public SheetProcessor sheet(Sheet sheet) {
    this.sheet = sheet;
    return this;
  }

  @Override
  public SheetProcessor sheetMeta(SheetMeta sheetMeta) {
    this.sheetMeta = sheetMeta;
    return this;
  }

  @Override
  public List<Object> process() {
    if (sheet == null) {
      throw new ExcelProcessException("set sheet first");
    }

    if (sheetMeta == null) {
      throw new ExcelProcessException("set sheet meta first");
    }

    if (objectFactory == null) {
      throw new ExcelProcessException("set object factory first");
    }

    List<FieldMeta> fieldMetas = sheetMeta.getFieldMetas();

    List<Object> oneSheetObjects = new ArrayList<>();
    sheetProcessorListener.beforeSheet(sheet);

    for (int i = sheetMeta.getDataStartRowIndex(); i <= sheet.sizeOfRows(); i++) {
      Row row = sheet.getRow(i);

      Object origin = objectFactory.create(row);
      sheetProcessorListener.beforeRow(row, origin);

      Object model = objectFactory.create(row);

      for (int j = 1; j <= row.sizeOfCells(); j++) {

        FieldMeta fieldMeta = fieldMetas.get(i);

        Cell cell = row.getCell(j);

        FieldValueSetter fieldValueSetter = key2fieldValueSetter.get(fieldMeta.getName());

        if (fieldValueSetter != null) {
          fieldValueSetter.set(model, cell, fieldMeta);
        } else {

          defaultValueSetter.set(model, cell, fieldMeta);
        }

      }

      sheetProcessorListener.afterRow(row, model);

      oneSheetObjects.add(model);
    }

    sheetProcessorListener.afterSheet(sheet, oneSheetObjects);

    return oneSheetObjects;
  }

}