package spread.sheet.w2o.processor;

import spread.sheet.model.core.SheetList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2017/1/4.
 */
public class DefaultWorkbookProcessor implements WorkbookProcessor {

  private List<SheetProcessor> sheetProcessors = new ArrayList<>();

  @Override
  public WorkbookProcessor sheetProcessor(SheetProcessor... sheetProcessors) {
    if (sheetProcessors == null) {
      return this;
    }
    Collections.addAll(this.sheetProcessors, sheetProcessors);
    return this;
  }

  @Override
  public List<SheetList<Object>> process() {
    List<SheetList<Object>> objects = new ArrayList<>();

    for (SheetProcessor sheetProcessor : sheetProcessors) {
      objects.add(sheetProcessor.process());
    }

    return objects;
  }
}