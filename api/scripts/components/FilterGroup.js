class FilterGroup extends Component{constructor(e){super(e),this.filterToggleRef=findRef(".filterToggleButton"),this.filtersContainerRef=findRef(".filtersContainer"),this.documentableFilterRef=findRef(".documentableFilter"),withEvent(this.filterToggleRef,"click",this.props.onFilterVisibilityChange),this.render(this.props)}onFilterClick=e=>{const{currentTarget:{dataset:{key:t,value:i}}}=e;this.props.onFilterToggle(t,i),e.stopPropagation(),e.preventDefault()};onSelectAllClick=({currentTarget:{dataset:{key:e}}})=>{this.props.onGroupSelectChange(e,!0)};onDeselectAllClick=e=>{this.props.onGroupSelectChange(e.currentTarget.dataset.key,!1),e.stopPropagation(),e.preventDefault()};onClearFilters=()=>{Object.entries(this.props.filter.filters).forEach((([e,t])=>this.props.onGroupSelectChange(e,!1)))};showPillDropdown=e=>{this.props.onPillClick(e.currentTarget.dataset.key),e.stopPropagation(),e.preventDefault()};hidePillDropdown=()=>{this.props.onPillCollapse()};attachFiltersClicks(){const e=findRefs("li.filterButtonItem",this.filtersContainerRef);attachListeners(e,"click",this.onFilterClick)}attachSelectingButtonsClicks(){const e=findRefs("button.selectAll",this.filtersContainerRef),t=findRefs("span.deselectAll",this.filtersContainerRef),i=findRefs("button.clearButton",this.documentableFilterRef),s=findRefs("div.pill",this.filtersContainerRef),l=findRefs("#main");attachListeners(e,"click",this.onSelectAllClick),attachListeners(t,"click",this.onDeselectAllClick),attachListeners(i,"click",this.onClearFilters),attachListeners(s,"click",this.showPillDropdown),attachListeners(l,"click",this.hidePillDropdown)}isActive(e){return e?"active":""}isVisible(e){return e?"visible":""}getSortedValues(e,t){const i=`${e.charAt(1).toLowerCase()}${e.slice(2)}`,s=Filter.defaultFilters[i];return Object.entries(t).sort((([e],[t])=>e===s?-1:t===s?1:e.localeCompare(t)))}getFirstSelected(e,t){const i=this.getSortedValues(e,t).find((([e,t])=>t.selected));return i?i[0]:""}getNumberOfSelectedFilters=(e,t)=>this.getSortedValues(e,t).reduce(((e,t)=>t[1].selected?e+1:e),0);getFilterGroup(e,t,i){const s=this.getFirstSelected(e,t),l=this.getNumberOfSelectedFilters(e,t),n=l>1?"+"+(l-1):"";return`\n      <div\n        class="pill-container body-small ${i===e?"menu-visible":""}"\n        tabindex="1"\n      >\n        <div class="pill ${l>0?"has-value":""}" data-key="${e}">\n          <span class="filter-name">${e.substring(1)}</span>\n          ${s} ${n}\n          <span\n            class="icon-button close deselectAll"\n            data-key="${e}"\n            />\n        </div>\n        <ul>\n          ${this.getSortedValues(e,t).map((([t,i])=>`<li\n                class="filterButtonItem  ${this.isVisible(i.visible)}"\n                data-selected="${i.selected}"\n                data-test-id="filterGroupButton"\n                data-key="${e}"\n                data-selected="${i.selected}"\n                data-value="${t}"\n                ${this.isActive(i.selected)}"\n              >\n              ${t}\n              </li>`)).join(" ")}\n        </ul>\n      </div>\n    `}render({filter:e,selectedPill:t}){attachDOM(this.filtersContainerRef,Object.entries(e.filters).filter((([e,t])=>Object.values(t).some((e=>e.visible)))).map((([e,i])=>this.getFilterGroup(e,i,t)))),this.attachFiltersClicks(),this.attachSelectingButtonsClicks()}}